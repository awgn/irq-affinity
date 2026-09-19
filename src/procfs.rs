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

/// Finds IRQs related to a specific network device.
/// Uses exact word matching, prefix matching (e.g. eth0-TxRx-0), or substring matching.
pub fn find_device_irqs(
    fs: &FsContext,
    dev: &str,
    num_cpus: usize,
) -> Result<Vec<IrqRecord>, ProcfsError> {
    let all = read_all_interrupts(fs, num_cpus)?;
    let dev_regex = regex::Regex::new(&format!(r"(?i)\b{}\b|{}-", regex::escape(dev), regex::escape(dev)))
        .map_err(|e| ProcfsError::InvalidInterruptLine(format!("regex error: {e}")))?;

    let matches: Vec<IrqRecord> = all
        .into_iter()
        .filter(|rec| dev_regex.is_match(&rec.description) || rec.description.contains(dev))
        .collect();

    Ok(matches)
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
