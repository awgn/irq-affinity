use std::path::PathBuf;
use std::process;

use clap::Parser;
use colored::*;
use irq_affinity::{
    apply_plan, find_device_irqs, get_nic_queues, plan_irq_binding, plan_xps_binding,
    read_all_interrupts, read_irq_affinity, read_topology, read_xps_affinity, AffinityError,
    AffinitySnapshot, CpuFilter, CpuMask, FsContext, Strategy, XpsFlavor,
};

#[derive(Parser, Debug)]
#[command(
    name = "irq-affinity",
    version,
    about = "Linux IRQ and XPS affinity binding tool and library",
    long_about = "A modern, reliable Linux interrupt and XPS network queue affinity tuning tool.\n\
                  Supports strategies: round-robin, compact[:N], all, manual.\n\
                  Supports configuration snapshots via --save and --load."
)]
struct Cli {
    /// Strategy for IRQ affinity (e.g. 'round-robin', 'compact:2', 'all')
    #[arg(short = 'i', long = "irq-strategy", value_name = "STRATEGY")]
    irq_strategy: Option<String>,

    /// Strategy for XPS affinity (e.g. 'round-robin', 'compact:2', 'all')
    #[arg(short = 'x', long = "xps-strategy", value_name = "STRATEGY")]
    xps_strategy: Option<String>,

    /// Flavor for XPS strategy ('cpu' or 'rxq')
    #[arg(short = 'f', long = "xps-flavor", value_name = "FLAVOR", default_value = "cpu")]
    xps_flavor: String,

    /// Explicit list or range of allowed CPUs (e.g. "0-3,8-11" or "0,2,4,6")
    #[arg(long = "cpus", value_name = "CPULIST")]
    cpus: Option<String>,

    /// First CPU involved in binding
    #[arg(long = "first-cpu", value_name = "INT")]
    first_cpu: Option<usize>,

    /// Range of CPUs involved in binding (MIN,MAX)
    #[arg(long = "range", value_name = "MIN,MAX", value_parser = parse_range)]
    range: Option<(usize, usize)>,

    /// Exclude specific CPUs from binding (can be repeated or comma-separated)
    #[arg(short = 'e', long = "exclude", value_name = "INT", value_delimiter = ',')]
    exclude: Vec<usize>,

    /// Apply the strategy only to the given physical package / NUMA socket ID
    #[arg(short = 'p', long = "package", value_name = "INT")]
    package: Option<usize>,

    /// Dry run, don't actually write to /proc or /sys
    #[arg(short = 'd', long = "dryrun")]
    dry_run: bool,

    /// Display IRQ mappings for all CPUs available
    #[arg(long = "show-all", alias = "show")]
    show_all: bool,

    /// Display IRQs and counters handled by the given CPU
    #[arg(long = "show-cpu", value_name = "CPU")]
    show_cpu: Option<usize>,

    /// Display IRQ details and counters for the specified device(s)
    #[arg(long = "show-irq")]
    show_irq: bool,

    /// Display XPS queue mappings for the specified device(s)
    #[arg(long = "show-xps")]
    show_xps: bool,

    /// Save current IRQ/XPS configuration of the specified devices to a JSON file
    #[arg(long = "save", value_name = "FILE")]
    save_file: Option<PathBuf>,

    /// Load and restore IRQ/XPS configuration from a JSON file
    #[arg(long = "load", value_name = "FILE")]
    load_file: Option<PathBuf>,

    /// Verbose diagnostic output
    #[arg(short = 'v', long = "verbose")]
    verbose: bool,

    /// Network devices to configure or inspect (e.g. eth0, eth1)
    #[arg(value_name = "DEVICES")]
    devices: Vec<String>,
}

fn parse_range(s: &str) -> Result<(usize, usize), String> {
    let parts: Vec<&str> = s.split(',').map(|p| p.trim()).collect();
    if parts.len() != 2 {
        return Err("range must be in MIN,MAX format (e.g. 0,15)".to_string());
    }
    let min: usize = parts[0]
        .parse()
        .map_err(|e| format!("invalid min range: {e}"))?;
    let max: usize = parts[1]
        .parse()
        .map_err(|e| format!("invalid max range: {e}"))?;
    if min > max {
        return Err(format!("range min ({min}) cannot be greater than max ({max})"));
    }
    Ok((min, max))
}

fn build_cpu_filter(cli: &Cli) -> Result<CpuFilter, String> {
    let allowed_cpus = if let Some(ref cpus_str) = cli.cpus {
        let mask = CpuMask::parse_cpu_list(cpus_str)
            .map_err(|e| format!("invalid --cpus specification '{cpus_str}': {e}"))?;
        Some(mask.to_cpus())
    } else {
        None
    };

    Ok(CpuFilter {
        allowed_cpus,
        first_cpu: cli.first_cpu,
        range: cli.range,
        exclude: cli.exclude.clone(),
        package: cli.package,
    })
}

fn main() {
    let cli = Cli::parse();
    let fs = FsContext::new();

    if let Err(err) = run(&cli, &fs) {
        eprintln!("{}: {}", "error".bold().red(), err);
        process::exit(1);
    }
}

fn run(cli: &Cli, fs: &FsContext) -> Result<(), Box<dyn std::error::Error>> {
    let flavor: XpsFlavor = cli.xps_flavor.parse().map_err(|e: String| e)?;

    // 1. Handle --save
    if let Some(ref save_path) = cli.save_file {
        if cli.devices.is_empty() {
            return Err("please specify at least one device to save configuration for".into());
        }
        let topo = read_topology(fs)?;
        let snapshot = AffinitySnapshot::capture_devices(fs, &topo, &cli.devices)?;
        snapshot.save_to_file(save_path)?;
        println!(
            "{} configuration for {:?} saved to {}",
            "Successfully".bold().green(),
            cli.devices,
            save_path.display()
        );
        return Ok(());
    }

    // 2. Handle --load
    if let Some(ref load_path) = cli.load_file {
        let snapshot = AffinitySnapshot::load_from_file(load_path)?;
        let target_devs = if cli.devices.is_empty() {
            None
        } else {
            Some(cli.devices.as_slice())
        };

        if cli.dry_run {
            println!("{}", "[Dry run mode - no changes will be applied]".yellow());
        }

        let count = snapshot.restore(fs, target_devs, cli.dry_run)?;
        println!(
            "{} restored {} affinity entries from {}",
            if cli.dry_run { "Simulated".yellow() } else { "Successfully".green() },
            count,
            load_path.display()
        );
        return Ok(());
    }

    // 3. Handle --show-all
    if cli.show_all {
        show_all_irqs(fs, &cli.devices)?;
        return Ok(());
    }

    // 4. Handle --show-cpu <CPU>
    if let Some(cpu) = cli.show_cpu {
        show_irqs_by_cpu(fs, cpu, &cli.devices)?;
        return Ok(());
    }

    // 5. Handle --show-irq
    if cli.show_irq {
        if cli.devices.is_empty() {
            return Err("please specify device(s) to inspect IRQ counters".into());
        }
        for dev in &cli.devices {
            show_device_irq_counters(fs, dev)?;
        }
        return Ok(());
    }

    // 6. Handle --show-xps
    if cli.show_xps {
        if cli.devices.is_empty() {
            return Err("please specify device(s) to inspect XPS mappings".into());
        }
        for dev in &cli.devices {
            show_device_xps(fs, dev, flavor)?;
        }
        return Ok(());
    }

    // 7. Handle --irq-strategy
    if let Some(ref strat_str) = cli.irq_strategy {
        if cli.devices.is_empty() {
            return Err("please specify device(s) to apply IRQ strategy to".into());
        }
        let strategy = Strategy::parse(strat_str)?;
        let filter = build_cpu_filter(cli)?;

        for dev in &cli.devices {
            apply_irq_strategy_to_nic(fs, dev, &strategy, &filter, cli.dry_run, cli.verbose)?;
        }
        return Ok(());
    }

    // 8. Handle --xps-strategy
    if let Some(ref strat_str) = cli.xps_strategy {
        if cli.devices.is_empty() {
            return Err("please specify device(s) to apply XPS strategy to".into());
        }
        let strategy = Strategy::parse(strat_str)?;
        let filter = build_cpu_filter(cli)?;

        for dev in &cli.devices {
            apply_xps_strategy_to_nic(fs, dev, &strategy, &filter, flavor, cli.dry_run, cli.verbose)?;
        }
        return Ok(());
    }

    // 9. Default: show current binding for specified devices
    if !cli.devices.is_empty() {
        for dev in &cli.devices {
            show_device_current_binding(fs, dev)?;
        }
    } else {
        println!("No action or device specified. Run with --help for usage.");
    }

    Ok(())
}

fn apply_irq_strategy_to_nic(
    fs: &FsContext,
    dev: &str,
    strategy: &Strategy,
    filter: &CpuFilter,
    dry_run: bool,
    verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let plan = plan_irq_binding(fs, dev, strategy, filter)?;

    if verbose {
        let topo = read_topology(fs)?;
        let eligible = filter.compute_eligible_cpus(&topo)?;
        println!(
            "Device {}: strategy={:?}, eligible_cpus={:?}",
            dev.bold(),
            strategy,
            eligible
        );
    }

    println!(
        "Binding IRQs for device {}:{}{}",
        dev.bold().cyan(),
        if dry_run { " (dry run)".yellow() } else { "".normal() },
        ""
    );

    for assign in &plan.assignments {
        let irq_num = match assign.target {
            irq_affinity::TargetKind::Irq { irq } => irq,
            _ => continue,
        };
        let cpus = assign.new_mask.to_cpus();
        let mask_hex = assign.new_mask.to_hex_string();

        print!(
            "  irq {} \u{2192} CPU {:?} {{mask = {}}} ",
            irq_num.to_string().bold().red(),
            cpus,
            mask_hex
        );

        if dry_run {
            println!(
                "{}",
                format!("[ /proc/irq/{irq_num}/smp_affinity <- {mask_hex} ]").dimmed()
            );
        } else {
            println!();
        }
    }

    apply_plan(&plan, fs, dry_run)?;
    Ok(())
}

fn apply_xps_strategy_to_nic(
    fs: &FsContext,
    dev: &str,
    strategy: &Strategy,
    filter: &CpuFilter,
    flavor: XpsFlavor,
    dry_run: bool,
    verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let plan = plan_xps_binding(fs, dev, strategy, filter, flavor)?;

    if verbose {
        let topo = read_topology(fs)?;
        let eligible = filter.compute_eligible_cpus(&topo)?;
        println!(
            "Device {}: strategy={:?}, eligible_cpus={:?}",
            dev.bold(),
            strategy,
            eligible
        );
    }

    println!(
        "Binding XPS ({}) queues for device {}:{}{}",
        flavor,
        dev.bold().cyan(),
        if dry_run { " (dry run)".yellow() } else { "".normal() },
        ""
    );

    for assign in &plan.assignments {
        let q = match assign.target {
            irq_affinity::TargetKind::XpsQueue { queue, .. } => queue,
            _ => continue,
        };
        let cpus = assign.new_mask.to_cpus();
        let mask_hex = assign.new_mask.to_hex_string();

        print!(
            "  queue {} \u{2192} CPU {:?} {{mask = {}}} ",
            q.to_string().bold().red(),
            cpus,
            mask_hex
        );

        if dry_run {
            println!(
                "{}",
                format!("[ /sys/class/net/{dev}/queues/tx-{q}/xps_{flavor}s <- {mask_hex} ]").dimmed()
            );
        } else {
            println!();
        }
    }

    apply_plan(&plan, fs, dry_run)?;
    Ok(())
}

fn show_device_current_binding(fs: &FsContext, dev: &str) -> Result<(), AffinityError> {
    let topo = read_topology(fs)?;
    let irqs = find_device_irqs(fs, dev, topo.num_processors())?;

    if irqs.is_empty() {
        return Err(AffinityError::NoQueuesOrIrqs(dev.to_string()));
    }

    let mut all_cpus = std::collections::BTreeSet::new();
    let mut details = Vec::new();

    for rec in &irqs {
        let mask = read_irq_affinity(fs, rec.irq).unwrap_or_default();
        let cpus = mask.to_cpus();
        for &c in &cpus {
            all_cpus.insert(c);
        }
        details.push((rec.irq, &rec.description, cpus));
    }

    let all_cpus_vec: Vec<usize> = all_cpus.into_iter().collect();
    println!(
        "IRQ binding for device {} on cpu {:?} ({} IRQs):",
        dev.bold().cyan(),
        all_cpus_vec,
        irqs.len()
    );

    for (irq, descr, cpus) in details {
        println!(
            "  irq {}:{} \u{2192} cpu {:?}",
            irq.to_string().bold().red(),
            descr.green(),
            cpus
        );
    }

    Ok(())
}

fn show_all_irqs(fs: &FsContext, filters: &[String]) -> Result<(), Box<dyn std::error::Error>> {
    let topo = read_topology(fs)?;
    let all = read_all_interrupts(fs, topo.num_processors())?;

    // Invert mapping: CPU -> list of IRQs
    let mut cpu_to_irqs: std::collections::BTreeMap<usize, Vec<(usize, String)>> =
        std::collections::BTreeMap::new();

    for rec in all {
        if !filters.is_empty()
            && !filters
                .iter()
                .any(|f| rec.description.to_lowercase().contains(&f.to_lowercase()))
        {
            continue;
        }

        if let Ok(mask) = read_irq_affinity(fs, rec.irq) {
            for cpu in mask.to_cpus() {
                cpu_to_irqs
                    .entry(cpu)
                    .or_default()
                    .push((rec.irq, rec.description.clone()));
            }
        }
    }

    for (cpu, irqs) in cpu_to_irqs {
        print!("  cpu {} \u{2192} ", cpu.to_string().bold().cyan());
        for (irq, descr) in irqs {
            print!("{}:{} ", irq.to_string().bold().red(), descr.green());
        }
        println!();
    }

    Ok(())
}

fn show_irqs_by_cpu(
    fs: &FsContext,
    cpu: usize,
    filters: &[String],
) -> Result<(), Box<dyn std::error::Error>> {
    let topo = read_topology(fs)?;
    let all = read_all_interrupts(fs, topo.num_processors())?;

    println!("CPU {}:", cpu.to_string().bold().cyan());

    for rec in all {
        if !filters.is_empty()
            && !filters
                .iter()
                .any(|f| rec.description.to_lowercase().contains(&f.to_lowercase()))
        {
            continue;
        }

        if let Ok(mask) = read_irq_affinity(fs, rec.irq) {
            if mask.contains(cpu) {
                let count = rec.counts.get(cpu).copied().unwrap_or(0);
                println!(
                    "  IRQ {}:{} \u{2192} {}",
                    rec.irq.to_string().bold().red(),
                    rec.description.green(),
                    count
                );
            }
        }
    }

    Ok(())
}

fn show_device_irq_counters(fs: &FsContext, dev: &str) -> Result<(), Box<dyn std::error::Error>> {
    // If `dev` parses as an IRQ number, show that single IRQ directly.
    // This makes `--show-irq 40` work as users expect, instead of treating
    // "40" as a network device name (which would yield "IRQ vector not found").
    if let Ok(irq_num) = dev.trim().parse::<usize>() {
        let topo = read_topology(fs)?;
        let all = read_all_interrupts(fs, topo.num_processors())?;
        if let Some(rec) = all.iter().find(|r| r.irq == irq_num) {
            let sum: u64 = rec.counts.iter().sum();
            let affinity = read_irq_affinity(fs, rec.irq)
                .map(|m| format!(" (affinity: cpu {:?}, mask {})", m.to_cpus(), m.to_hex_string()))
                .unwrap_or_default();
            println!("IRQ {}:{} \u{2192} {}{} {:?}",
                rec.irq.to_string().bold().red(),
                rec.description.green(),
                sum,
                affinity,
                rec.counts
            );
            return Ok(());
        }
        return Err(format!("IRQ {irq_num} not found in /proc/interrupts!").into());
    }

    let topo = read_topology(fs)?;
    let irqs = find_device_irqs(fs, dev, topo.num_processors())?;

    if irqs.is_empty() {
        return Err(format!("IRQ vector not found for dev {dev}!").into());
    }

    println!("IRQ binding for device {dev}:");
    let mut total: u64 = 0;

    for rec in &irqs {
        let sum: u64 = rec.counts.iter().sum();
        total += sum;
        println!(
            "  irq {}:{} \u{2192} {} {:?}",
            rec.irq.to_string().bold().red(),
            rec.description.green(),
            sum,
            rec.counts
        );
    }

    println!("  irq total: {total}");
    Ok(())
}

fn show_device_xps(
    fs: &FsContext,
    dev: &str,
    flavor: XpsFlavor,
) -> Result<(), Box<dyn std::error::Error>> {
    let queues = get_nic_queues(fs, dev)?;
    println!(
        "{} ({} queues, kind '{}'):",
        dev.bold().cyan(),
        queues.tx_queues,
        flavor
    );

    for q in 0..queues.tx_queues {
        let mask = read_xps_affinity(fs, dev, q, flavor).unwrap_or_default();
        let cpus = mask.to_cpus();
        println!(
            "  {} \u{2192} cpu {:?}",
            format!("Tx-{q}").bold().red(),
            cpus
        );
    }

    Ok(())
}
