# Tjr_pcache

[TOC]

## Introduction

This is a persistent cache, part of ImpFS. It maintains a persistent
list of map operations, which avoids actually executing these
operations against the B-tree.

There are two libraries: `tjr_pcache` for the core library, and `tjr_pcache_example` for the example.

There is an executable `run_dmap_example.exe` . See the Makefile for how to run.

## Quick links

* ocamldoc, see https://tomjridge.github.io/tjr_pcache/

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
make -k run_dmap_example 
/usr/bin/time -f real %e, user %U, sys %S _build/default/bin/run_dmap_example.exe 1e6
|  Total time   |  wp1  |  wp2  |  count     |  Unit cost  |
|          787  |   zb  |   ab  |         1  |        787  |
|       836717  |   cd  |   ab  |      2678  |        312  |
|      1136435  |   ab  |   ac  |      5357  |        212  |
|      9784823  |   za  |   ab  |      2678  |       3653  |
|     31060580  |   ac  |   bc  |      5357  |       5798  |
|     56638041  |   zb  |   za  |    999999  |         56  |
|     57930322  |   bc  |   cd  |      5357  |      10813  |
|    618748790  |   cd  |   zb  |      2678  |     231048  |
|   1619279773  |   za  |   zb  |    997322  |       1623  |
Profiling, test_dmap_ops_on_file: 2681274591
Profiling, run_dmap_example: 2681281162
read back 1000000 ops
Profiling, read_back: 340871909
real 3.05, user 2.89, sys 0.14
~~~

This involves logging 1e6 insert operations and then reading them back in. The resulting store takes 11MB on disk. Profiling timings are in nanoseconds, using Jane Street Core.Time_stamp_counter bindings.

## Dependencies (core library)

| package                     | Comment                 |
| --------------------------- | ----------------------- |
| yojson, ppx_deriving_yojson | For debugging           |
| ppx_bin_prot                | For marshalling to disk |
| tjr_fs_shared               | ImpFS shared library    |

## Dependencies (example)

In addition to core:

| Opam package | Comment                                            |
| ------------ | -------------------------------------------------- |
| core         | Jane Street core lib, for working with buffers etc |
| tjr_profile  | Performance testing                                |



## Rough performance measurements

Note: these tests are based on running the run_dmap_example.exe example, which currently only writes full blocks (or the last block when closing), and does not attempt to force data to disk using eg fsync.

| Count (number of operations) | Time (s) | File size |
| ---------------------------- | -------- | --------- |
| 1e6                          | 2.67     | 11MB      |
| 2e6                          | 5.15     | 22MB      |



### Device info

The backing storage is provided by a file on ext4, with the underlying device reported (via nvme list) as:

~~~
Node             SN                   Model                                    Namespace Usage                      Format           FW Rev  
---------------- -------------------- ---------------------------------------- --------- -------------------------- ---------------- --------
/dev/nvme0n1     S3WTNF0K859497       SAMSUNG MZVLB512HAJQ-000H1               1         201.69  GB / 512.11  GB    512   B +  0 B   EXA73H1Q

~~~

