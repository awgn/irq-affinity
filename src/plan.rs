use std::fmt;
use crate::mask::CpuMask;
use crate::procfs::{write_irq_affinity, write_xps_affinity, FsContext, ProcfsError, XpsFlavor};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TargetKind {
    Irq { irq: usize },
    XpsQueue { queue: usize, flavor: XpsFlavor },
}

impl fmt::Display for TargetKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TargetKind::Irq { irq } => write!(f, "IRQ {irq}"),
            TargetKind::XpsQueue { queue, flavor } => write!(f, "Tx-{queue} ({flavor})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingAssignment {
    pub target: TargetKind,
    pub description: String,
    pub current_mask: Option<CpuMask>,
    pub new_mask: CpuMask,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingPlan {
    pub device: String,
    pub assignments: Vec<BindingAssignment>,
}

impl BindingPlan {
    pub fn new(device: impl Into<String>) -> Self {
        Self {
            device: device.into(),
            assignments: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.assignments.is_empty()
    }

    pub fn len(&self) -> usize {
        self.assignments.len()
    }

    /// Applies the planned affinity changes to the system.
    /// If dry_run is true, no files are modified.
    pub fn apply(&self, fs: &FsContext, dry_run: bool) -> Result<(), ProcfsError> {
        for assign in &self.assignments {
            match assign.target {
                TargetKind::Irq { irq } => {
                    write_irq_affinity(fs, irq, &assign.new_mask, dry_run)?;
                }
                TargetKind::XpsQueue { queue, flavor } => {
                    write_xps_affinity(fs, &self.device, queue, flavor, &assign.new_mask, dry_run)?;
                }
            }
        }
        Ok(())
    }
}
