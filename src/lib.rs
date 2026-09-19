pub mod config;
pub mod mask;
pub mod plan;
pub mod procfs;
pub mod strategy;
pub mod topology;

pub use config::{AffinitySnapshot, IrqConfig, NicConfig, XpsQueueConfig};
pub use mask::{CpuMask, ParseMaskError};
pub use plan::{BindingAssignment, BindingPlan, TargetKind};
pub use procfs::{
    find_device_irqs, get_nic_queues, read_all_interrupts, read_irq_affinity, read_topology,
    read_xps_affinity, write_irq_affinity, write_xps_affinity, FsContext, IrqRecord, NicQueues,
    ProcfsError, XpsFlavor,
};
pub use strategy::{generate_masks, CpuFilter, Strategy, StrategyError};
pub use topology::{CpuInfo, Topology, TopologyError};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum AffinityError {
    #[error("procfs error: {0}")]
    Procfs(#[from] ProcfsError),
    #[error("strategy error: {0}")]
    Strategy(#[from] StrategyError),
    #[error("no IRQs or queues found for device '{0}'")]
    NoQueuesOrIrqs(String),
}

/// Computes an IRQ binding plan for a single network device.
///
/// This function is pure with respect to affinity writes: it queries the device
/// interrupts and current affinity, computes the new assignments according to
/// the chosen strategy and CPU filters, and returns a `BindingPlan`.
pub fn plan_irq_binding(
    fs: &FsContext,
    dev: &str,
    strategy: &Strategy,
    filter: &CpuFilter,
) -> Result<BindingPlan, AffinityError> {
    let topo = read_topology(fs)?;
    let eligible_cpus = filter.compute_eligible_cpus(&topo)?;
    let irq_records = find_device_irqs(fs, dev, topo.num_processors())?;

    if irq_records.is_empty() {
        return Err(AffinityError::NoQueuesOrIrqs(dev.to_string()));
    }

    let masks = generate_masks(strategy, &eligible_cpus, irq_records.len())?;

    let mut plan = BindingPlan::new(dev);
    for (rec, new_mask) in irq_records.into_iter().zip(masks) {
        let current_mask = read_irq_affinity(fs, rec.irq).ok();
        plan.assignments.push(BindingAssignment {
            target: TargetKind::Irq { irq: rec.irq },
            description: rec.description,
            current_mask,
            new_mask,
        });
    }

    Ok(plan)
}

/// Computes an XPS transmit-queue binding plan for a single network device.
pub fn plan_xps_binding(
    fs: &FsContext,
    dev: &str,
    strategy: &Strategy,
    filter: &CpuFilter,
    flavor: XpsFlavor,
) -> Result<BindingPlan, AffinityError> {
    let topo = read_topology(fs)?;
    let eligible_cpus = filter.compute_eligible_cpus(&topo)?;
    let queues = get_nic_queues(fs, dev)?;

    if queues.tx_queues == 0 {
        return Err(AffinityError::NoQueuesOrIrqs(dev.to_string()));
    }

    let masks = generate_masks(strategy, &eligible_cpus, queues.tx_queues)?;

    let mut plan = BindingPlan::new(dev);
    for (q, new_mask) in (0..queues.tx_queues).zip(masks) {
        let current_mask = read_xps_affinity(fs, dev, q, flavor).ok();
        plan.assignments.push(BindingAssignment {
            target: TargetKind::XpsQueue { queue: q, flavor },
            description: format!("Tx-{q} ({flavor})"),
            current_mask,
            new_mask,
        });
    }

    Ok(plan)
}

/// Applies a previously computed BindingPlan to the system.
/// If dry_run is true, verifies and simulates without writing to the filesystem.
pub fn apply_plan(
    plan: &BindingPlan,
    fs: &FsContext,
    dry_run: bool,
) -> Result<(), AffinityError> {
    plan.apply(fs, dry_run).map_err(AffinityError::Procfs)
}
