# $Id$

VERSION = 1.0

SHELL=/bin/sh
INSTALL = install
SHARE = /usr/share/emacs
SITELISP = $(SHARE)/site-lisp/x-$(VERSION)
EMACS=$(EMACSPATH)/emacs
PWD=$(shell pwd)
MAKE_AUTOLOADS=/usr/local/bin/make-autoloads

.PHONY: FORCE

SOURCES=*.el
SHIPS=$(SOURCES)
TARGETS := $(SOURCES:.el=.elc)

all: .autoloads

sources:
	@echo $(SOURCES)

ship: compile
	rm -rf $(SITELISP)
	mkdir -p $(SITELISP)
	install -m 444 $(TARGETS) $(SITELISP)
	$(MAKE_AUTOLOADS) --top $(SITELISP) -c $(SOURCES) > .autoloads.ship
	install -m 444 .autoloads.ship $(SITELISP)/.autoloads

.xz.dat: $(SOURCES)
	xz -ywqn $(SOURCES)

clean: $(FORCE)
	rm -f .autoloads .autoloads.ship *.elc

compile: $(SHIPS)
	$(EMACS) --batch --load=byte-compile-directory.el -e batch-byte-compile $^
