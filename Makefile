build:
	$(MAKE) -C src build
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
