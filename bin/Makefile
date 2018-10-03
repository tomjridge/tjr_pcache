build: run_tests.native

run_tests.native:
	$(MAKE) clean
	ocamlbuild -quiet -use-ocamlfind -tag thread -pkg tjr_pcache run_tests.native

run_tests:
	./run_tests.native

clean:
	ocamlbuild -clean
