TMP_DOC_DIR:=/tmp/tjr_pcache
scratch:=/tmp/l/github/scratch

default: all
	dune build example/run_dmap_example.exe

-include Makefile.ocaml

run_dmap_example:
	time dune exec example/run_dmap_example.exe 1e5

# test: FORCE
# 	dune build test/test_main.exe
# 
# run_test:
# 	dune exec test/test_main.exe 1 6

# for auto-completion of Makefile target
clean::

