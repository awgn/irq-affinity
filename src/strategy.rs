use thiserror::Error;

use crate::mask::CpuMask;
use crate::topology::Topology;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum StrategyError {
    #[error("no eligible CPUs found after applying filters")]
    NoEligibleCpus,
    #[error("compact multiplier must be greater than zero")]
    InvalidCompactMultiplier,
    #[error("manual assignment requires at least one CPU or mask")]
    EmptyManualAssignment,
}

/// Strategy for distributing IRQs or Queues across CPUs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Strategy {
    /// Cycles through eligible CPUs one-by-one (equivalent to Compact(1)).
    RoundRobin,
    /// Packs up to N queues per CPU before advancing to the next CPU.
    Compact(usize),
    /// Assigns ALL eligible CPUs to every queue/IRQ simultaneously.
    All,
    /// Explicit, manual assignment of CPU masks per queue/IRQ.
    Manual(Vec<CpuMask>),
}

impl Strategy {
    /// Parses a strategy string like "round-robin", "compact", "compact:2", "all".
    pub fn parse(s: &str) -> Result<Self, String> {
        let s = s.trim().to_lowercase();
        if s == "round-robin" || s == "roundrobin" || s == "rr" {
            Ok(Strategy::RoundRobin)
        } else if s == "all" {
            Ok(Strategy::All)
        } else if s == "compact" {
            Ok(Strategy::Compact(1))
        } else if let Some((name, val)) = s.split_once(':').or_else(|| s.split_once('/')) {
            if name == "compact" || name == "raster" || name == "multiple" {
                let m: usize = val
                    .parse()
                    .map_err(|_| format!("invalid compact multiplier: '{val}'"))?;
                if m == 0 {
                    return Err("compact multiplier must be > 0".to_string());
                }
                Ok(Strategy::Compact(m))
            } else {
                Err(format!("unknown strategy prefix: '{name}'"))
            }
        } else {
            Err(format!(
                "unknown strategy '{s}' (valid choices: round-robin, compact, compact:N, all)"
            ))
        }
    }
}

/// Filtering criteria to select eligible CPUs from the system topology.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CpuFilter {
    /// Explicit list of allowed CPUs (e.g. from --cpus 0,2,4,6 or 0-7)
    pub allowed_cpus: Option<Vec<usize>>,
    /// Starting CPU offset
    pub first_cpu: Option<usize>,
    /// Range of allowed CPU IDs (inclusive: min, max)
    pub range: Option<(usize, usize)>,
    /// CPUs to explicitly exclude
    pub exclude: Vec<usize>,
    /// Restrict to a specific physical socket / NUMA package ID
    pub package: Option<usize>,
}

impl CpuFilter {
    /// Computes the ordered list of eligible CPU IDs matching the criteria.
    pub fn compute_eligible_cpus(&self, topology: &Topology) -> Result<Vec<usize>, StrategyError> {
        let base_cpus = if let Some(ref allowed) = self.allowed_cpus {
            allowed.clone()
        } else {
            topology.processor_ids()
        };

        let mut filtered: Vec<usize> = base_cpus
            .into_iter()
            .filter(|&cpu| {
                // Check range
                if let Some((min, max)) = self.range {
                    if cpu < min || cpu > max {
                        return false;
                    }
                }
                // Check exclude list
                if self.exclude.contains(&cpu) {
                    return false;
                }
                // Check package
                if let Some(pkg) = self.package {
                    if topology.package_of(cpu) != Some(pkg) {
                        return false;
                    }
                }
                true
            })
            .collect();

        if filtered.is_empty() {
            return Err(StrategyError::NoEligibleCpus);
        }

        // Apply starting CPU offset if specified
        if let Some(start) = self.first_cpu {
            if let Some(pos) = filtered.iter().position(|&cpu| cpu >= start) {
                // Rotate the list so it begins at 'start' (or next available)
                filtered.rotate_left(pos);
            }
        }

        Ok(filtered)
    }
}

/// Generates a sequence of CpuMask assignments for `count` items (IRQs or queues)
/// based on the chosen strategy and eligible CPUs.
pub fn generate_masks(
    strategy: &Strategy,
    eligible_cpus: &[usize],
    count: usize,
) -> Result<Vec<CpuMask>, StrategyError> {
    if count == 0 {
        return Ok(Vec::new());
    }

    match strategy {
        Strategy::All => {
            if eligible_cpus.is_empty() {
                return Err(StrategyError::NoEligibleCpus);
            }
            let full_mask = CpuMask::from_cpus(eligible_cpus.iter().copied());
            Ok(vec![full_mask; count])
        }

        Strategy::RoundRobin => generate_masks(&Strategy::Compact(1), eligible_cpus, count),

        Strategy::Compact(multi) => {
            if *multi == 0 {
                return Err(StrategyError::InvalidCompactMultiplier);
            }
            if eligible_cpus.is_empty() {
                return Err(StrategyError::NoEligibleCpus);
            }

            let mut masks = Vec::with_capacity(count);
            let mut cpu_idx = 0;
            let mut repeat_count = 0;

            for _ in 0..count {
                let cpu = eligible_cpus[cpu_idx % eligible_cpus.len()];
                masks.push(CpuMask::from_cpus([cpu]));

                repeat_count += 1;
                if repeat_count >= *multi {
                    repeat_count = 0;
                    cpu_idx += 1;
                }
            }

            Ok(masks)
        }

        Strategy::Manual(custom_masks) => {
            if custom_masks.is_empty() {
                return Err(StrategyError::EmptyManualAssignment);
            }
            let mut masks = Vec::with_capacity(count);
            for i in 0..count {
                masks.push(custom_masks[i % custom_masks.len()].clone());
            }
            Ok(masks)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_round_robin() {
        let cpus = vec![0, 2, 4];
        let masks = generate_masks(&Strategy::RoundRobin, &cpus, 5).unwrap();
        let assigned_cpus: Vec<Vec<usize>> = masks.iter().map(|m| m.to_cpus()).collect();
        assert_eq!(
            assigned_cpus,
            vec![vec![0], vec![2], vec![4], vec![0], vec![2]]
        );
    }

    #[test]
    fn test_compact() {
        let cpus = vec![0, 1];
        let masks = generate_masks(&Strategy::Compact(2), &cpus, 5).unwrap();
        let assigned_cpus: Vec<Vec<usize>> = masks.iter().map(|m| m.to_cpus()).collect();
        assert_eq!(
            assigned_cpus,
            vec![vec![0], vec![0], vec![1], vec![1], vec![0]]
        );
    }

    #[test]
    fn test_all_strategy() {
        let cpus = vec![0, 1, 2, 3];
        let masks = generate_masks(&Strategy::All, &cpus, 3).unwrap();
        assert_eq!(masks.len(), 3);
        for m in masks {
            assert_eq!(m.to_cpus(), vec![0, 1, 2, 3]);
        }
    }
}
