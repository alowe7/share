# $Id$ 

INSTALL = install
LOCALBIN = /usr/local/bin

.PHONY: FORCE

all:

make-autoloads: FORCE
	autoconf configure.ac > configure
	chmod +x configure
	./configure
	chmod +x make-autoloads

ship: make-autoloads
	$(INSTALL) -m 755 make-autoloads $(LOCALBIN)


test: clean mintest plustest

mintest: make-autoloads
	./make-autoloads --top=/whatever --prefix=s ./test/mintest.el > ./test/mintest.out
	diff -q ./test/mintest.out ./test/mintest.good

plustest: make-autoloads
	./make-autoloads --top=/whatever --prefix=s ./test/plustest.el > ./test/plustest.out
	diff -q ./test/plustest.out ./test/plustest.good

clean: FORCE
	rm -f ./test/*.out 
	rm -f make-autoloads
	rm -f configure config.status config.log 
	rm -rf autom4te.cache