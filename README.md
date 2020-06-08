# Tjr_pcache

[TOC]

## Introduction

This is a persistent cache, part of ImpFS. It maintains a persistent
list of map operations, which avoids actually executing these
operations against the B-tree.

There are two libraries: `tjr_pcache` for the core library, and `tjr_pcache_example` for the example.

There is an executable `run_pcache_example.exe` . See the Makefile for how to run.

## Quick links

* ocamldoc, see https://tomjridge.github.io/ocamldocs/

## Install

To build and install using dune, type `make`. This assumes you have already installed the dependencies.

Alternatively, just opam pin this repo and the dependencies:

~~~
opam install -y dune ocamlfind odoc
opam pin add -y -n tjr_lib_core https://github.com/tomjridge/tjr_lib.git
opam pin add -y -n tjr_lib https://github.com/tomjridge/tjr_lib.git
opam pin add -y -n tjr_monad https://github.com/tomjridge/tjr_monad.git
opam pin add -y -n tjr_profile https://github.com/tomjridge/tjr_profile.git
opam pin add -y -n tjr_fs_shared https://github.com/tomjridge/tjr_fs_shared.git
opam pin add -y -n tjr_pcache https://github.com/tomjridge/tjr_pcache.git
opam pin add -y -n tjr_pcache_example https://github.com/tomjridge/tjr_pcache.git
opam install -y tjr_pcache tjr_pcache_example
~~~


## Example

Running the example gives output similar to the following:

~~~
make -k run_pcache_example 
time dune exec bin/run_pcache_example.exe pcache.store 1e6
run_pcache_example 875983269
Blk write count (src-example/dmap_example.ml): 8034

real	0m1.028s
user	0m0.972s
sys	0m0.045s

~~~

This involves logging 1e6 insert operations. The resulting store takes 11MB on disk. Profiling timings are in nanoseconds, using Jane Street Core.Time_stamp_counter bindings. Overall, the time taken is 875983269 nanoseconds, or about 0.9s.

## Dependencies (core library)

| package                               | Comment                   |
| ------------------------------------- | ------------------------- |
| yojson, ppx_deriving_yojson, alcotest | For testing and debugging |
| ppx_bin_prot                          | For marshalling to disk   |
| ppx_optcomp                           | Conditional compilation   |
| tjr_profile                           | Profiling                 |
| tjr_fs_shared                         | ImpFS shared library      |

## Dependencies (example)

In addition to core:

| Opam package | Comment                                                      |
| ------------ | ------------------------------------------------------------ |
| core         | Jane Street core lib, for working with buffers etc; for TSC-based timing. |



## Rough performance measurements

Note: these tests are based on running the run_pcache_example.exe example, which currently only writes full blocks (or the last block when closing), and does not attempt to force data to disk using eg fsync.

| Count (number of operations) | Time (s) | File size |
| ---------------------------- | -------- | --------- |
| 1e6                          | 0.90     | 11MB      |
| 10e6                         | 9.0      | 22MB      |



### Device info

The backing storage is provided by a file on ext4, with the underlying device reported (via nvme list) as:

~~~
Node             SN                   Model                                    Namespace Usage                      Format           FW Rev  
---------------- -------------------- ---------------------------------------- --------- -------------------------- ---------------- --------
/dev/nvme0n1     S3WTNF0K859497       SAMSUNG MZVLB512HAJQ-000H1               1         201.69  GB / 512.11  GB    512   B +  0 B   EXA73H1Q

~~~

