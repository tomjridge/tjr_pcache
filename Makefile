all:
	$(MAKE) -C src

clean:
	$(MAKE) -C src clean
	$(MAKE) -C bin clean

check:
	$(MAKE) -C bin
