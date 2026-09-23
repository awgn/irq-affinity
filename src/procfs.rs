use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use thiserror::Error;

use crate::mask::CpuMask;
use crate::topology::{Topology, TopologyError};

#[derive(Error, Debug)]
pub enum ProcfsError {
    #[error("I/O error at {path}: {source}")]
    Io {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
    #[error("failed to parse topology: {0}")]
    Topology(#[from] TopologyError),
    #[error("failed to parse mask from {path}: {source}")]
    ParseMask {
        path: PathBuf,
        #[source]
        source: crate::mask::ParseMaskError,
    },
    #[error("invalid interrupt line format: {0}")]
    InvalidInterruptLine(String),
    #[error("device '{0}' not found or has no IRQs/queues")]
    DeviceNotFound(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XpsFlavor {
    #[default]
    Cpu,
    Rxq,
}

impl fmt::Display for XpsFlavor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            XpsFlavor::Cpu => write!(f, "cpu"),
            XpsFlavor::Rxq => write!(f, "rxq"),
        }
    }
}

impl std::str::FromStr for XpsFlavor {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "cpu" | "cpus" => Ok(XpsFlavor::Cpu),
            "rxq" | "rxqs" => Ok(XpsFlavor::Rxq),
            other => Err(format!("unknown XPS flavor '{other}' (expected 'cpu' or 'rxq')")),
        }
    }
}

/// Filesystem context, abstracting root path for testing on non-Linux or mock environments.
#[derive(Debug, Clone)]
pub struct FsContext {
    root: PathBuf,
}

impl Default for FsContext {
    fn default() -> Self {
        Self {
            root: PathBuf::from("/"),
        }
    }
}

impl FsContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_root(root: impl AsRef<Path>) -> Self {
        Self {
            root: root.as_ref().to_path_buf(),
        }
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn proc_cpuinfo_path(&self) -> PathBuf {
        self.root.join("proc/cpuinfo")
    }

    pub fn proc_interrupts_path(&self) -> PathBuf {
        self.root.join("proc/interrupts")
    }

    pub fn proc_irq_smp_affinity_path(&self, irq: usize) -> PathBuf {
        self.root.join(format!("proc/irq/{irq}/smp_affinity"))
    }

    pub fn sys_net_queues_path(&self, dev: &str) -> PathBuf {
        self.root.join(format!("sys/class/net/{dev}/queues"))
    }

    pub fn sys_net_device_path(&self, dev: &str) -> PathBuf {
        self.root.join(format!("sys/class/net/{dev}/device"))
    }

    pub fn sys_net_xps_path(&self, dev: &str, queue: usize, flavor: XpsFlavor) -> PathBuf {
        self.root.join(format!(
            "sys/class/net/{dev}/queues/tx-{queue}/xps_{flavor}s"
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrqRecord {
    pub irq: usize,
    pub counts: Vec<u64>,
    pub description: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NicQueues {
    pub device: String,
    pub tx_queues: usize,
    pub rx_queues: usize,
}

/// Reads and parses topology from /proc/cpuinfo
pub fn read_topology(fs: &FsContext) -> Result<Topology, ProcfsError> {
    let path = fs.proc_cpuinfo_path();
    let content = fs::read_to_string(&path).map_err(|e| ProcfsError::Io {
        path: path.clone(),
        source: e,
    })?;
    Topology::parse_cpuinfo(&content).map_err(ProcfsError::Topology)
}

/// Reads all IRQ entries from /proc/interrupts
pub fn read_all_interrupts(
    fs: &FsContext,
    num_cpus: usize,
) -> Result<Vec<IrqRecord>, ProcfsError> {
    let path = fs.proc_interrupts_path();
    let content = fs::read_to_string(&path).map_err(|e| ProcfsError::Io {
        path: path.clone(),
        source: e,
    })?;

    let mut records = Vec::new();

    for line in content.lines() {
        let trimmed = line.trim_start();
        if trimmed.is_empty() {
            continue;
        }

        // Only process lines that start with an integer followed by ':'
        if let Some((irq_str, rest)) = trimmed.split_once(':') {
            if let Ok(irq) = irq_str.trim().parse::<usize>() {
                // Fields in rest: count_0, count_1, ..., [interrupt_controller], [device_description...]
                let tokens: Vec<&str> = rest.split_whitespace().collect();
                let mut counts = Vec::with_capacity(num_cpus);

                let mut idx = 0;
                while idx < tokens.len() && idx < num_cpus {
                    if let Ok(c) = tokens[idx].parse::<u64>() {
                        counts.push(c);
                        idx += 1;
                    } else {
                        break;
                    }
                }

                // Any remaining tokens form the description
                let description = tokens[idx..].join(" ");

                records.push(IrqRecord {
                    irq,
                    counts,
                    description,
                });
            }
        }
    }

    Ok(records)
}

/// Discovers the bus identifier for a network device (e.g. PCI slot name like "0000:cc:00.1").
///
/// Inspects `/sys/class/net/<dev>/device` symlink target as well as `uevent` files.
pub fn get_device_bus(fs: &FsContext, dev: &str) -> Option<String> {
    let device_path = fs.sys_net_device_path(dev);

    // 1. Try reading the symlink target of /sys/class/net/<dev>/device
    if let Ok(target) = fs::read_link(&device_path) {
        if let Some(file_name) = target
            .file_name()
            .and_then(|s| s.to_str())
            .filter(|s| !s.is_empty() && *s != "." && *s != "..")
        {
            return Some(file_name.to_string());
        }
    }

    // 2. Try reading /sys/class/net/<dev>/device/uevent for PCI_SLOT_NAME=...
    let uevent_path = device_path.join("uevent");
    if let Ok(content) = fs::read_to_string(&uevent_path) {
        for line in content.lines() {
            if let Some(slot) = line.strip_prefix("PCI_SLOT_NAME=") {
                let slot = slot.trim();
                if !slot.is_empty() {
                    return Some(slot.to_string());
                }
            }
        }
    }

    // 3. Try reading /sys/class/net/<dev>/uevent as fallback
    let net_uevent = fs.root().join(format!("sys/class/net/{dev}/uevent"));
    if let Ok(content) = fs::read_to_string(&net_uevent) {
        for line in content.lines() {
            if let Some(slot) = line.strip_prefix("PCI_SLOT_NAME=") {
                let slot = slot.trim();
                if !slot.is_empty() {
                    return Some(slot.to_string());
                }
            }
        }
    }

    None
}

/// Filters interrupt records by a device name or bus pattern.
fn filter_interrupts(records: &[IrqRecord], pattern: &str) -> Result<Vec<IrqRecord>, ProcfsError> {
    let dev_regex = regex::Regex::new(&format!(
        r"(?i)\b{}\b|{}-",
        regex::escape(pattern),
        regex::escape(pattern)
    ))
    .map_err(|e| ProcfsError::InvalidInterruptLine(format!("regex error: {e}")))?;

    let pattern_lower = pattern.to_lowercase();
    let matches: Vec<IrqRecord> = records
        .iter()
        .filter(|rec| {
            dev_regex.is_match(&rec.description)
                || rec.description.to_lowercase().contains(&pattern_lower)
        })
        .cloned()
        .collect();

    Ok(matches)
}

/// Finds IRQs related to a specific network device.
///
/// First attempts to match by the network interface name (e.g. "eth0").
/// If no matching IRQs are found (which is common for vendors like Mellanox where
/// `/proc/interrupts` lists bus identifiers such as `mlx5_comp61@pci:0000:cc:00.1`),
/// it attempts to discover the device's bus identifier from sysfs and repeats the
/// matching procedure using the bus address.
pub fn find_device_irqs(
    fs: &FsContext,
    dev: &str,
    num_cpus: usize,
) -> Result<Vec<IrqRecord>, ProcfsError> {
    let all = read_all_interrupts(fs, num_cpus)?;

    // 1. Try matching with the device name directly (e.g. "eth0")
    let matches = filter_interrupts(&all, dev)?;
    if !matches.is_empty() {
        return Ok(matches);
    }

    // 2. If no matches found, attempt to find the hardware bus (e.g. PCI slot name) and match on that
    if let Some(bus) = get_device_bus(fs, dev) {
        let bus_matches = filter_interrupts(&all, &bus)?;
        if !bus_matches.is_empty() {
            return Ok(bus_matches);
        }

        // Also try without PCI domain prefix if applicable (e.g. "0000:cc:00.1" -> "cc:00.1")
        if let Some(short_bus) = bus.strip_prefix("0000:") {
            let short_matches = filter_interrupts(&all, short_bus)?;
            if !short_matches.is_empty() {
                return Ok(short_matches);
            }
        }
    }

    Ok(Vec::new())
}

/// Reads the current smp_affinity of an IRQ
pub fn read_irq_affinity(fs: &FsContext, irq: usize) -> Result<CpuMask, ProcfsError> {
    let path = fs.proc_irq_smp_affinity_path(irq);
    let s = fs::read_to_string(&path).map_err(|e| ProcfsError::Io {
        path: path.clone(),
        source: e,
    })?;
    CpuMask::parse_hex(&s).map_err(|e| ProcfsError::ParseMask {
        path: path.clone(),
        source: e,
    })
}

/// Writes the smp_affinity of an IRQ (or prints in dry_run mode)
pub fn write_irq_affinity(
    fs: &FsContext,
    irq: usize,
    mask: &CpuMask,
    dry_run: bool,
) -> Result<(), ProcfsError> {
    let path = fs.proc_irq_smp_affinity_path(irq);
    let mask_str = mask.to_hex_string();
    if !dry_run {
        fs::write(&path, format!("{mask_str}\n")).map_err(|e| ProcfsError::Io {
            path: path.clone(),
            source: e,
        })?;
    }
    Ok(())
}

/// Inspects /sys/class/net/<dev>/queues to count tx and rx queues
pub fn get_nic_queues(fs: &FsContext, dev: &str) -> Result<NicQueues, ProcfsError> {
    let path = fs.sys_net_queues_path(dev);
    if !path.exists() {
        return Ok(NicQueues {
            device: dev.to_string(),
            tx_queues: 0,
            rx_queues: 0,
        });
    }

    let mut tx_count = 0;
    let mut rx_count = 0;

    let entries = fs::read_dir(&path).map_err(|e| ProcfsError::Io {
        path: path.clone(),
        source: e,
    })?;

    for entry in entries {
        let entry = entry.map_err(|e| ProcfsError::Io {
            path: path.clone(),
            source: e,
        })?;
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if name_str.starts_with("tx-") {
            tx_count += 1;
        } else if name_str.starts_with("rx-") {
            rx_count += 1;
        }
    }

    Ok(NicQueues {
        device: dev.to_string(),
        tx_queues: tx_count,
        rx_queues: rx_count,
    })
}

/// Reads the current XPS mask for a transmit queue
pub fn read_xps_affinity(
    fs: &FsContext,
    dev: &str,
    queue: usize,
    flavor: XpsFlavor,
) -> Result<CpuMask, ProcfsError> {
    let path = fs.sys_net_xps_path(dev, queue, flavor);
    let s = fs::read_to_string(&path).map_err(|e| ProcfsError::Io {
        path: path.clone(),
        source: e,
    })?;
    CpuMask::parse_hex(&s).map_err(|e| ProcfsError::ParseMask {
        path: path.clone(),
        source: e,
    })
}

/// Writes the XPS mask for a transmit queue
pub fn write_xps_affinity(
    fs: &FsContext,
    dev: &str,
    queue: usize,
    flavor: XpsFlavor,
    mask: &CpuMask,
    dry_run: bool,
) -> Result<(), ProcfsError> {
    let path = fs.sys_net_xps_path(dev, queue, flavor);
    let mask_str = mask.to_hex_string();
    if !dry_run {
        fs::write(&path, format!("{mask_str}\n")).map_err(|e| ProcfsError::Io {
            path: path.clone(),
            source: e,
        })?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_find_device_irqs_direct_match() {
        let temp = TempDir::new().unwrap();
        let fs = FsContext::with_root(temp.path());

        let proc_dir = temp.path().join("proc");
        fs::create_dir_all(&proc_dir).unwrap();

        let interrupts_content = r#"
            CPU0       CPU1
  40:        100          0   PCI-MSI-edge      eth0-TxRx-0
  41:          0        200   PCI-MSI-edge      eth0-TxRx-1
  50:          5          0   IO-APIC-edge      timer
"#;
        fs::write(fs.proc_interrupts_path(), interrupts_content).unwrap();

        let irqs = find_device_irqs(&fs, "eth0", 2).unwrap();
        assert_eq!(irqs.len(), 2);
        assert_eq!(irqs[0].irq, 40);
        assert_eq!(irqs[1].irq, 41);
    }

    #[test]
    fn test_find_device_irqs_via_pci_bus_symlink() {
        let temp = TempDir::new().unwrap();
        let fs = FsContext::with_root(temp.path());

        let proc_dir = temp.path().join("proc");
        fs::create_dir_all(&proc_dir).unwrap();

        // Mellanox style line with bus in controller and description, but not interface name
        let interrupts_content = r#"
            CPU0       CPU1
 665:     219828          2   IR-PCI-MSIX-0000:cc:00.1   62-edge      mlx5_comp61@pci:0000:cc:00.1
 666:       1234          0   IR-PCI-MSIX-0000:cc:00.1   63-edge      mlx5_comp62@pci:0000:cc:00.1
"#;
        fs::write(fs.proc_interrupts_path(), interrupts_content).unwrap();

        // Create sys/class/net/ens1f0np0/device symlink pointing to ../../../0000:cc:00.1
        let net_dir = temp.path().join("sys/class/net/ens1f0np0");
        fs::create_dir_all(&net_dir).unwrap();

        #[cfg(unix)]
        {
            std::os::unix::fs::symlink("../../../0000:cc:00.1", net_dir.join("device")).unwrap();
        }

        let bus = get_device_bus(&fs, "ens1f0np0");
        assert_eq!(bus.as_deref(), Some("0000:cc:00.1"));

        let irqs = find_device_irqs(&fs, "ens1f0np0", 2).unwrap();
        assert_eq!(irqs.len(), 2);
        assert_eq!(irqs[0].irq, 665);
        assert_eq!(irqs[1].irq, 666);
    }

    #[test]
    fn test_find_device_irqs_via_pci_bus_uevent() {
        let temp = TempDir::new().unwrap();
        let fs = FsContext::with_root(temp.path());

        let proc_dir = temp.path().join("proc");
        fs::create_dir_all(&proc_dir).unwrap();

        let interrupts_content = r#"
            CPU0       CPU1
 665:     219828          2   IR-PCI-MSIX-0000:cc:00.1   62-edge      mlx5_comp61@pci:0000:cc:00.1
"#;
        fs::write(fs.proc_interrupts_path(), interrupts_content).unwrap();

        // Create sys/class/net/ens1f0np0/device/uevent with PCI_SLOT_NAME
        let device_dir = temp.path().join("sys/class/net/ens1f0np0/device");
        fs::create_dir_all(&device_dir).unwrap();
        fs::write(
            device_dir.join("uevent"),
            "DRIVER=mlx5_core\nPCI_SLOT_NAME=0000:cc:00.1\n",
        )
        .unwrap();

        let bus = get_device_bus(&fs, "ens1f0np0");
        assert_eq!(bus.as_deref(), Some("0000:cc:00.1"));

        let irqs = find_device_irqs(&fs, "ens1f0np0", 2).unwrap();
        assert_eq!(irqs.len(), 1);
        assert_eq!(irqs[0].irq, 665);
    }
}
