# $Id$

VERSION = 1.0

SHELL=/bin/sh
INSTALL = install
SHARE = /usr/share/emacs
SITELISP = $(SHARE)/site-lisp/x-$(VERSION)

.PHONY: FORCE

SOURCES=*.el

all: .autoloads

sources:
	@echo $(SOURCES)

ship: .autoloads
ifeq ($(SITELISP),)
	@echo "SITELISP not set.  run make from top level directory to ship"
else
	rm -rf $(SITELISP)
	mkdir -p $(SITELISP)
	install -m 444 $(SOURCES) $(SITELISP)
	make-autoloads --top $(SITELISP) $(SOURCES) > .autoloads.ship
	install -m 444 .autoloads.ship $(SITELISP)/.autoloads
endif

.xz.dat: $(SOURCES)
	xz -ywqn $(SOURCES)

.autoloads: FORCE
	make-autoloads $(SOURCES) > .autoloads

clean: $(FORCE)
	rm -f .autoloads .autoloads.ship
