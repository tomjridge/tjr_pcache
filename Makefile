store:=pcache.store

default: 
	$(MAKE) all

all::
#	dune build bin/run_pcache_example.exe

update_generated_doc::
	cd src && (ocamldoc_pyexpander pcache_intf.ml)
	cd src && (ocamldoc_pyexpander make.ml)
	cd src && (ocamldoc_pyexpander summary.t.ml > summary.ml)


tmp: FORCE
	dune build --only-packages tjr_pcache @install
#	$(MAKE) docs
#	dune build bin/debug.exe
#	dune build bin/run_pcache_tests.exe

-include Makefile.ocaml

run_pcache_example: 
	time dune exec bin/run_pcache_example.exe $(store) 1e6
	ls -alh $(store)

debug:
	dune exec bin/debug.exe $(store) 

run_tests:
	dune exec bin/run_pcache_tests.exe test 

# for auto-completion of Makefile target
clean::
	rm -f $(store)
