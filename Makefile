build:
	jbuilder build
	cp _build/install/default/bin/merlin Merlin.native

test:
	jbuilder runtest

all: build test

clean:
	jbuilder clean
	rm Merlin.native

install:
	jbuilder install

uninstall:
	jbuilder uninstall

.PHONY: build test all install uninstall clean
