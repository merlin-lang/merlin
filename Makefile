build:
	jbuilder build
	cp _build/install/default/bin/merlin Merlin.native

test:
	-jbuilder runtest --no-buffer
	-@cp -r _build/default/test/_build/_tests/ _build/

all: build test

clean:
	jbuilder clean
	rm Merlin.native

install:
	jbuilder install

uninstall:
	jbuilder uninstall

.PHONY: build test all install uninstall clean
