use std::fs;
use tempfile::TempDir;

use irq_affinity::{
    apply_plan, get_nic_queues, plan_irq_binding, plan_xps_binding, read_irq_affinity,
    read_xps_affinity, AffinitySnapshot, CpuFilter, CpuMask, FsContext, Strategy, XpsFlavor,
};

fn setup_mock_environment() -> (TempDir, FsContext) {
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create /proc
    fs::create_dir_all(root.join("proc/irq/40")).unwrap();
    fs::create_dir_all(root.join("proc/irq/41")).unwrap();
    fs::create_dir_all(root.join("proc/irq/42")).unwrap();
    fs::create_dir_all(root.join("proc/irq/43")).unwrap();

    // Create /sys/class/net/eth0/queues
    fs::create_dir_all(root.join("sys/class/net/eth0/queues/tx-0")).unwrap();
    fs::create_dir_all(root.join("sys/class/net/eth0/queues/tx-1")).unwrap();
    fs::create_dir_all(root.join("sys/class/net/eth0/queues/rx-0")).unwrap();
    fs::create_dir_all(root.join("sys/class/net/eth0/queues/rx-1")).unwrap();

    // Write cpuinfo (4 CPUs across 2 sockets)
    let cpuinfo = r#"
processor	: 0
physical id	: 0
core id		: 0

processor	: 1
physical id	: 0
core id		: 1

processor	: 2
physical id	: 1
core id		: 0

processor	: 3
physical id	: 1
core id		: 1
"#;
    fs::write(root.join("proc/cpuinfo"), cpuinfo).unwrap();

    // Write interrupts
    let interrupts = r#"
            CPU0       CPU1       CPU2       CPU3
  40:        100          0          0          0   PCI-MSI-edge      eth0-TxRx-0
  41:          0        200          0          0   PCI-MSI-edge      eth0-TxRx-1
  42:          0          0        300          0   PCI-MSI-edge      eth0-TxRx-2
  43:          0          0          0        400   PCI-MSI-edge      eth0-TxRx-3
"#;
    fs::write(root.join("proc/interrupts"), interrupts).unwrap();

    // Write initial smp_affinity masks
    fs::write(root.join("proc/irq/40/smp_affinity"), "00000001\n").unwrap();
    fs::write(root.join("proc/irq/41/smp_affinity"), "00000001\n").unwrap();
    fs::write(root.join("proc/irq/42/smp_affinity"), "00000001\n").unwrap();
    fs::write(root.join("proc/irq/43/smp_affinity"), "00000001\n").unwrap();

    // Write initial xps masks
    fs::write(
        root.join("sys/class/net/eth0/queues/tx-0/xps_cpus"),
        "00000001\n",
    )
    .unwrap();
    fs::write(
        root.join("sys/class/net/eth0/queues/tx-1/xps_cpus"),
        "00000001\n",
    )
    .unwrap();

    let fs_ctx = FsContext::with_root(root);
    (temp, fs_ctx)
}

#[test]
fn test_plan_and_apply_round_robin() {
    let (_temp, fs) = setup_mock_environment();

    let filter = CpuFilter::default();
    let plan = plan_irq_binding(&fs, "eth0", &Strategy::RoundRobin, &filter).unwrap();

    assert_eq!(plan.device, "eth0");
    assert_eq!(plan.assignments.len(), 4);

    // Round robin across CPUs 0, 1, 2, 3
    assert_eq!(plan.assignments[0].new_mask.to_cpus(), vec![0]);
    assert_eq!(plan.assignments[1].new_mask.to_cpus(), vec![1]);
    assert_eq!(plan.assignments[2].new_mask.to_cpus(), vec![2]);
    assert_eq!(plan.assignments[3].new_mask.to_cpus(), vec![3]);

    // Apply plan
    apply_plan(&plan, &fs, false).unwrap();

    // Verify written masks
    assert_eq!(read_irq_affinity(&fs, 40).unwrap().to_cpus(), vec![0]);
    assert_eq!(read_irq_affinity(&fs, 41).unwrap().to_cpus(), vec![1]);
    assert_eq!(read_irq_affinity(&fs, 42).unwrap().to_cpus(), vec![2]);
    assert_eq!(read_irq_affinity(&fs, 43).unwrap().to_cpus(), vec![3]);
}

#[test]
fn test_plan_and_apply_all_strategy() {
    let (_temp, fs) = setup_mock_environment();

    let filter = CpuFilter::default();
    let plan = plan_irq_binding(&fs, "eth0", &Strategy::All, &filter).unwrap();

    assert_eq!(plan.assignments.len(), 4);
    for assign in &plan.assignments {
        assert_eq!(assign.new_mask.to_cpus(), vec![0, 1, 2, 3]);
    }

    apply_plan(&plan, &fs, false).unwrap();

    for irq in [40, 41, 42, 43] {
        assert_eq!(
            read_irq_affinity(&fs, irq).unwrap().to_cpus(),
            vec![0, 1, 2, 3]
        );
    }
}

#[test]
fn test_plan_compact_and_filter_package() {
    let (_temp, fs) = setup_mock_environment();

    // Filter only socket/package 1 (CPUs 2 and 3)
    let filter = CpuFilter {
        package: Some(1),
        ..Default::default()
    };

    // Compact(2): 2 queues per CPU
    let plan = plan_irq_binding(&fs, "eth0", &Strategy::Compact(2), &filter).unwrap();

    assert_eq!(plan.assignments.len(), 4);
    assert_eq!(plan.assignments[0].new_mask.to_cpus(), vec![2]);
    assert_eq!(plan.assignments[1].new_mask.to_cpus(), vec![2]);
    assert_eq!(plan.assignments[2].new_mask.to_cpus(), vec![3]);
    assert_eq!(plan.assignments[3].new_mask.to_cpus(), vec![3]);
}

#[test]
fn test_plan_xps_binding() {
    let (_temp, fs) = setup_mock_environment();

    let queues = get_nic_queues(&fs, "eth0").unwrap();
    assert_eq!(queues.tx_queues, 2);
    assert_eq!(queues.rx_queues, 2);

    let filter = CpuFilter::default();
    let plan =
        plan_xps_binding(&fs, "eth0", &Strategy::RoundRobin, &filter, XpsFlavor::Cpu).unwrap();

    assert_eq!(plan.assignments.len(), 2);
    assert_eq!(plan.assignments[0].new_mask.to_cpus(), vec![0]);
    assert_eq!(plan.assignments[1].new_mask.to_cpus(), vec![1]);

    apply_plan(&plan, &fs, false).unwrap();

    assert_eq!(
        read_xps_affinity(&fs, "eth0", 0, XpsFlavor::Cpu)
            .unwrap()
            .to_cpus(),
        vec![0]
    );
    assert_eq!(
        read_xps_affinity(&fs, "eth0", 1, XpsFlavor::Cpu)
            .unwrap()
            .to_cpus(),
        vec![1]
    );
}

#[test]
fn test_save_and_load_configuration() {
    let (temp, fs) = setup_mock_environment();
    let topo = irq_affinity::read_topology(&fs).unwrap();

    // Capture state
    let snapshot = AffinitySnapshot::capture_devices(&fs, &topo, &["eth0".to_string()]).unwrap();
    let save_path = temp.path().join("saved_affinity.json");
    snapshot.save_to_file(&save_path).unwrap();

    // Verify file was written
    assert!(save_path.exists());

    // Modify IRQs to something else
    let mask_custom = CpuMask::from_cpus([3]);
    irq_affinity::write_irq_affinity(&fs, 40, &mask_custom, false).unwrap();
    assert_eq!(read_irq_affinity(&fs, 40).unwrap().to_cpus(), vec![3]);

    // Load and restore
    let loaded = AffinitySnapshot::load_from_file(&save_path).unwrap();
    let restored_count = loaded.restore(&fs, None, false).unwrap();
    assert!(restored_count > 0);

    // Verify IRQ 40 was restored to initial mask [0]
    assert_eq!(read_irq_affinity(&fs, 40).unwrap().to_cpus(), vec![0]);
}
