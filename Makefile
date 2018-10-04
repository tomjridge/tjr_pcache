SHELL:=bash

build:
	$(MAKE) -C src build


# NOTE must install before building bin
build_bin:
	$(MAKE) -C bin build

install:
	$(MAKE) -C src install

uninstall:
	$(MAKE) -C src uninstall

clean:
	$(MAKE) -C src clean
	$(MAKE) -C bin clean

check:
	$(MAKE) -C bin
