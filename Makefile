TMP_DOC_DIR:=/tmp/tjr_pcache
scratch:=/tmp/l/github/scratch

default: all

all::
	dune build --only-packages tjr_pcache @install
	dune build bin/run_dmap_example.exe
	dune build bin/run_tests.exe

-include Makefile.ocaml

run_dmap_example:
	time dune exec bin/run_dmap_example.exe 1e6

run_tests:
	dune exec bin/run_tests.exe test 

# test: FORCE
# 	dune build test/test_main.exe
# 
# run_test:
# 	dune exec test/test_main.exe 1 6

# for auto-completion of Makefile target
clean::
	rm -f dmap_example.store
