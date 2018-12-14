DUNE:=dune

build:
	$(DUNE) build @install
	$(DUNE) build bin/run_dmap_tests.exe
	$(DUNE) build bin/run_dmap_example.exe
#	$(DUNE) build bin/test_dcl.exe
#	$(DUNE) build @all #tjr_pcache_test
#	$(DUNE) build bin/run_tests.exe bin/test_dcl.exe

install:
	$(DUNE) install

clean:
	$(DUNE) clean
	rm -f dune-project tjr_pcache.install # FIXME?
	rm -f dmap_example.store # test file

doc: FORCE
	$(DUNE) build @doc

view_doc:
	google-chrome  _build/default/_doc/_html/index.html


run_tests:
	$(DUNE) exec bin/run_dmap_tests.exe
	$(DUNE) exec bin/run_dmap_example.exe
#	$(DUNE) exec bin/run_tests.exe
#	$(DUNE) exec bin/test_dcl.exe

# 
# run_more_tests:
# 	$(DUNE) exec test/test_main.exe 1 10



FORCE:
