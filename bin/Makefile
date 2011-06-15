# $Id$ 

INSTALL = install
LOCALBIN = /usr/local/bin

.PHONY: FORCE

ship: FORCE
	$(INSTALL) -m 755 make-autoloads $(LOCALBIN)


test: clean mintest plustest

mintest: FORCE
	./make-autoloads --top=/whatever --prefix=s ./test/mintest.el > ./test/mintest.out
	diff -q ./test/mintest.out ./test/mintest.good

plustest: FORCE
	./make-autoloads --top=/whatever --prefix=s ./test/plustest.el > ./test/plustest.out
	diff -q ./test/plustest.out ./test/plustest.good

clean: FORCE
	rm -f ./test/*.out
