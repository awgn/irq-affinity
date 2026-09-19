use std::collections::BTreeMap;
use std::fs;
use std::path::Path;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::mask::CpuMask;
use crate::procfs::{
    find_device_irqs, get_nic_queues, read_irq_affinity, read_xps_affinity,
    write_irq_affinity, write_xps_affinity, FsContext, ProcfsError, XpsFlavor,
};
use crate::topology::Topology;

#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("I/O error at {path}: {source}")]
    Io {
        path: String,
        #[source]
        source: std::io::Error,
    },
    #[error("JSON serialization error: {0}")]
    Json(#[from] serde_json::Error),
    #[error("Procfs error: {0}")]
    Procfs(#[from] ProcfsError),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct IrqConfig {
    pub irq: usize,
    pub description: String,
    pub affinity: CpuMask,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct XpsQueueConfig {
    pub queue: usize,
    pub affinity: CpuMask,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct NicConfig {
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub irqs: Vec<IrqConfig>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub xps_cpu: Vec<XpsQueueConfig>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub xps_rxq: Vec<XpsQueueConfig>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AffinitySnapshot {
    pub version: u32,
    pub devices: BTreeMap<String, NicConfig>,
}

impl Default for AffinitySnapshot {
    fn default() -> Self {
        Self {
            version: 1,
            devices: BTreeMap::new(),
        }
    }
}

impl AffinitySnapshot {
    pub fn new() -> Self {
        Self::default()
    }

    /// Captures the current configuration of the specified network devices.
    pub fn capture_devices(
        fs: &FsContext,
        topology: &Topology,
        devices: &[String],
    ) -> Result<Self, ConfigError> {
        let mut snapshot = Self::new();

        for dev in devices {
            let mut nic_cfg = NicConfig::default();

            // 1. Capture IRQs
            if let Ok(irqs) = find_device_irqs(fs, dev, topology.num_processors()) {
                for rec in irqs {
                    if let Ok(mask) = read_irq_affinity(fs, rec.irq) {
                        nic_cfg.irqs.push(IrqConfig {
                            irq: rec.irq,
                            description: rec.description,
                            affinity: mask,
                        });
                    }
                }
            }

            // 2. Capture XPS
            if let Ok(queues) = get_nic_queues(fs, dev) {
                for q in 0..queues.tx_queues {
                    if let Ok(mask) = read_xps_affinity(fs, dev, q, XpsFlavor::Cpu) {
                        nic_cfg.xps_cpu.push(XpsQueueConfig {
                            queue: q,
                            affinity: mask,
                        });
                    }
                    if let Ok(mask) = read_xps_affinity(fs, dev, q, XpsFlavor::Rxq) {
                        nic_cfg.xps_rxq.push(XpsQueueConfig {
                            queue: q,
                            affinity: mask,
                        });
                    }
                }
            }

            snapshot.devices.insert(dev.clone(), nic_cfg);
        }

        Ok(snapshot)
    }

    /// Saves snapshot to a JSON file.
    pub fn save_to_file(&self, path: impl AsRef<Path>) -> Result<(), ConfigError> {
        let path_ref = path.as_ref();
        let content = serde_json::to_string_pretty(self)?;
        fs::write(path_ref, content).map_err(|e| ConfigError::Io {
            path: path_ref.display().to_string(),
            source: e,
        })?;
        Ok(())
    }

    /// Loads snapshot from a JSON file.
    pub fn load_from_file(path: impl AsRef<Path>) -> Result<Self, ConfigError> {
        let path_ref = path.as_ref();
        let content = fs::read_to_string(path_ref).map_err(|e| ConfigError::Io {
            path: path_ref.display().to_string(),
            source: e,
        })?;
        let snapshot: Self = serde_json::from_str(&content)?;
        Ok(snapshot)
    }

    /// Restores the configuration from this snapshot.
    pub fn restore(
        &self,
        fs: &FsContext,
        target_devices: Option<&[String]>,
        dry_run: bool,
    ) -> Result<usize, ProcfsError> {
        let mut count = 0;

        for (dev, cfg) in &self.devices {
            if let Some(targets) = target_devices {
                if !targets.is_empty() && !targets.iter().any(|t| t == dev) {
                    continue;
                }
            }

            // Restore IRQs
            for irq_item in &cfg.irqs {
                write_irq_affinity(fs, irq_item.irq, &irq_item.affinity, dry_run)?;
                count += 1;
            }

            // Restore XPS CPU
            for xps in &cfg.xps_cpu {
                write_xps_affinity(fs, dev, xps.queue, XpsFlavor::Cpu, &xps.affinity, dry_run)?;
                count += 1;
            }

            // Restore XPS Rxq
            for xps in &cfg.xps_rxq {
                write_xps_affinity(fs, dev, xps.queue, XpsFlavor::Rxq, &xps.affinity, dry_run)?;
                count += 1;
            }
        }

        Ok(count)
    }
}
