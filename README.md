Irq Affinity
============

An irq affinity Linux tool.

Build
-----

This tool is a stack project. 

`stack build`

For static build: 

`stack install --flag irq-affinity:StaticBuild`


Build and Install
-----------------

```
$ stack install --flag irq-affinity:StaticBuild

Copying from /home/osboxes/irq-affinity/.stack-work/install/x86_64-linux-tinfo6/ea6a5512af5c690390d6b9dcf14f3a435f578b97c30a30953ddddf7fd82c0277/9.2.5/bin/irq-affinity to /home/osboxes/.local/bin/irq-affinity

Copied executables to /home/user/.local/bin:
- irq-affinity

```

Usage
-----

```
irq-affinity v2.0: a Linux interrupt affinity binding tool.

irq-affinity [OPTIONS] [ITEM]

Common flags:
  -f --firstcpu=INT     First CPU involved in binding.
     --strategy=NAME    Strategies: basic, round-robin, multiple/n, raster/n,
                        even, odd, any, all-in:id, step:id, custom:step/multi.
     --one-to-many      Bind each IRQ to every eligible CPU. Note: by default
                        irq affinity is set one-to-one.
     --show --showall   Display IRQs for all CPUs available.
  -d --dryrun           Dry run, don't actually set IRQ affinity.
     --bind=ITEM        Set the IRQs affinity of the given device (e.g.,
                        --bind eth0 1 2).
Filters:
  -e --exclude=INT      Exclude CPUs from binding.
  -p --package=INT      Apply then strategy to the given package (physical
                        id).
  -r --range=MIN,MAX    Range of CPUs involved in binding.
Display:
     --cpu=INT          Display IRQs of the given CPUs set.
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number
```
