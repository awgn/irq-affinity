use std::collections::BTreeSet;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TopologyError {
    #[error("failed to parse cpuinfo: {0}")]
    ParseError(String),
    #[error("no processors found in cpuinfo")]
    NoProcessorsFound,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CpuInfo {
    pub processor: usize,
    pub physical_id: usize,
    pub core_id: usize,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Topology {
    cpus: Vec<CpuInfo>,
}

impl Topology {
    /// Creates a Topology by parsing /proc/cpuinfo content.
    pub fn parse_cpuinfo(content: &str) -> Result<Self, TopologyError> {
        let mut cpus = Vec::new();

        let mut curr_proc: Option<usize> = None;
        let mut curr_phys: Option<usize> = None;
        let mut curr_core: Option<usize> = None;

        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() {
                if let Some(p) = curr_proc.take() {
                    cpus.push(CpuInfo {
                        processor: p,
                        physical_id: curr_phys.unwrap_or(0),
                        core_id: curr_core.unwrap_or(0),
                    });
                    curr_phys = None;
                    curr_core = None;
                }
                continue;
            }

            if let Some((key, val)) = line.split_once(':') {
                let key = key.trim();
                let val = val.trim();

                if key == "processor" {
                    if let Some(p) = curr_proc.take() {
                        cpus.push(CpuInfo {
                            processor: p,
                            physical_id: curr_phys.unwrap_or(0),
                            core_id: curr_core.unwrap_or(0),
                        });
                        curr_phys = None;
                        curr_core = None;
                    }
                    curr_proc = Some(val.parse().map_err(|e| {
                        TopologyError::ParseError(format!("invalid processor id: {e}"))
                    })?);
                } else if key == "physical id" {
                    curr_phys = val.parse().ok();
                } else if key == "core id" {
                    curr_core = val.parse().ok();
                }
            }
        }

        if let Some(p) = curr_proc {
            cpus.push(CpuInfo {
                processor: p,
                physical_id: curr_phys.unwrap_or(0),
                core_id: curr_core.unwrap_or(0),
            });
        }

        if cpus.is_empty() {
            return Err(TopologyError::NoProcessorsFound);
        }

        // Sort by processor id
        cpus.sort_by_key(|c| c.processor);

        Ok(Self { cpus })
    }

    /// Creates a synthetic topology with N CPUs and single socket (useful for tests or fallback).
    pub fn synthetic(num_cpus: usize) -> Self {
        let cpus = (0..num_cpus)
            .map(|i| CpuInfo {
                processor: i,
                physical_id: 0,
                core_id: i,
            })
            .collect();
        Self { cpus }
    }

    pub fn num_processors(&self) -> usize {
        self.cpus.len()
    }

    pub fn cpus(&self) -> &[CpuInfo] {
        &self.cpus
    }

    pub fn processor_ids(&self) -> Vec<usize> {
        self.cpus.iter().map(|c| c.processor).collect()
    }

    pub fn get_cpu(&self, processor: usize) -> Option<&CpuInfo> {
        self.cpus.iter().find(|c| c.processor == processor)
    }

    pub fn package_of(&self, processor: usize) -> Option<usize> {
        self.get_cpu(processor).map(|c| c.physical_id)
    }

    pub fn packages(&self) -> Vec<usize> {
        let pkgs: BTreeSet<usize> = self.cpus.iter().map(|c| c.physical_id).collect();
        pkgs.into_iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_CPUINFO: &str = r#"
processor	: 0
vendor_id	: GenuineIntel
cpu family	: 6
model		: 158
physical id	: 0
core id		: 0

processor	: 1
vendor_id	: GenuineIntel
physical id	: 0
core id		: 1

processor	: 2
vendor_id	: GenuineIntel
physical id	: 1
core id		: 0
"#;

    #[test]
    fn test_parse_cpuinfo() {
        let topo = Topology::parse_cpuinfo(SAMPLE_CPUINFO).unwrap();
        assert_eq!(topo.num_processors(), 3);
        assert_eq!(topo.packages(), vec![0, 1]);
        assert_eq!(topo.package_of(0), Some(0));
        assert_eq!(topo.package_of(1), Some(0));
        assert_eq!(topo.package_of(2), Some(1));
    }
}
