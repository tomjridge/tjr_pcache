TMP_DOC_DIR:=/tmp/tjr_pcache
scratch:=/tmp/l/github/scratch

default: all

all::
	dune build bin/run_dmap_example.exe

-include Makefile.ocaml

run_dmap_example:
	time dune exec bin/run_dmap_example.exe 1e6

# test: FORCE
# 	dune build test/test_main.exe
# 
# run_test:
# 	dune exec test/test_main.exe 1 6

# for auto-completion of Makefile target
clean::
	rm -f dmap_example.store
