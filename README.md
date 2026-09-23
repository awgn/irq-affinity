# irq-affinity (Rust Edition)

A fast, reliable Linux IRQ and XPS network queue affinity tuning tool and Rust library.

## Features

- **Dual-mode architecture**:
  - Standalone high-performance CLI binary (`irq-affinity`)
  - Clean, reusable library API (`irq_affinity`)
- **Strict single-NIC model**: Internal binding logic computes assignments for **one NIC at a time**, ensuring deterministic, pure, and easily testable behavior.
- **Simplified Strategies**:
  - `round-robin`: distributes queues/interrupts evenly across eligible CPUs.
  - `compact[:N]`: packs up to $N$ queues per CPU before advancing to the next.
  - `all`: binds each queue/interrupt to **all** eligible CPUs simultaneously.
  - `manual`: allows explicit mask or CPU assignments.
- **Topology-aware**:
  - Filter by NUMA socket / physical package (`--package <ID>`).
  - Restrict to CPU ranges or lists (`--cpus 0-3,8-11`).
  - Offset starting CPU (`--first-cpu <ID>`).
  - Exclude isolated CPUs (`--exclude 0,1`).
- **State Snapshots (`--save` and `--load`)**:
  - Save current configuration of devices to structured JSON.
  - Restore configuration with `--load file.json` (supports `--dryrun`).
- **Zero arbitrary CPU limits**:
  - `CpuMask` is implemented as an unbounded bitset, scaling smoothly to 4096+ cores.
- **Mock-friendly filesystem abstraction**:
  - All kernel interactions (`/proc` and `/sys`) are mediated through `FsContext`, allowing full integration testing even on macOS and non-root CI environments.

---

## Installation & Build

```bash
cargo build --release
```

The resulting binary will be at `target/release/irq-affinity`.

### Static musl build (recommended for minimal container / production Linux)

```bash
rustup target add x86_64-unknown-linux-musl
cargo build --release --target x86_64-unknown-linux-musl
```

---

## CLI Usage

### Inspect Current Binding

```bash
# Show current IRQ bindings for eth0
irq-affinity eth0

# Show IRQ activity for CPU 2
irq-affinity --show-cpu 2

# Show XPS queue mappings for eth0
irq-affinity --show-xps eth0
```

### Apply Binding Strategies

```bash
# Round-robin across all available CPUs (dry run)
irq-affinity -i round-robin --dryrun eth0

# Compact: 2 queues per core, only on NUMA package 0
irq-affinity -i compact:2 --package 0 eth0

# Bind all queues to all cores in range 4-15
irq-affinity -i all --cpus 4-15 eth0

# Apply XPS transmit queue affinity
irq-affinity -x round-robin --package 1 eth0
```

### Save and Load Configuration Snapshots

```bash
# Save snapshot of eth0 and eth1
irq-affinity --save /etc/irq-affinity/backup.json eth0 eth1

# Preview restoration (dry run)
irq-affinity --load /etc/irq-affinity/backup.json --dryrun

# Restore snapshot
irq-affinity --load /etc/irq-affinity/backup.json
```

---

## Library API Example

```rust
use irq_affinity::{
    FsContext, Strategy, CpuFilter,
    plan_irq_binding, apply_plan,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let fs = FsContext::new();

    // 1. Configure CPU selection
    let filter = CpuFilter {
        package: Some(0), // Socket 0 only
        ..Default::default()
    };

    // 2. Compute plan for a single NIC
    let plan = plan_irq_binding(&fs, "eth0", &Strategy::RoundRobin, &filter)?;

    for assign in &plan.assignments {
        println!("{}: CPUs {:?}", assign.description, assign.new_mask.to_cpus());
    }

    // 3. Apply changes (set dry_run to true for preview)
    apply_plan(&plan, &fs, false)?;

    Ok(())
}
```
