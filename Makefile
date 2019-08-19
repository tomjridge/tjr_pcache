TMP_DOC_DIR:=/tmp/tjr_pcache
scratch:=/tmp/l/github/scratch
store:=pcache.store

default: all

all::
	dune build --only-packages tjr_pcache @install
	dune build bin/run_pcache_example.exe
	dune build bin/run_pcache_tests.exe

-include Makefile.ocaml

run_pcache_example: 
	time dune exec bin/run_pcache_example.exe $(store) 1e6
	ls -alh $(store)

run_tests:
	dune exec bin/run_pcache_tests.exe test 

# for auto-completion of Makefile target
clean::
	rm -f $(store)
