# Tjr_pcache

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
time dune exec bin/run_dmap_example.exe 1e6
|  Total time | wp1 | wp2 |    count | Unit cost |
|    64328180 |  zb |  za |   999999 |        64 |
|  3991701496 |  za |  zb |  1000000 |      3991 |
test_dmap_ops_on_file finished, total time: 4778045960
run_dmap_example finished, total time: 4778123543
read back 1000000 ops
read finished, total time: 643479978
|  Total time | wp1 | wp2 |    count | Unit cost |
|     1869584 |  ab |  ac |     5357 |       348 |
|    49128723 |  ac |  bc |     5357 |      9170 |
|    80151300 |  bc |  cd |     5357 |     14961 |
|  3917891014 |  cd |  ab |     5356 |    731495 |

real	0m1.896s
user	0m1.785s
sys	0m0.100s

~~~

This involves logging 1e6 insert operations and then reading them back in. The resulting store takes 11MB on disk.

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
