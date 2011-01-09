# $Id$

VERSION = 1.0

SHELL=/bin/sh
INSTALL = install
SHARE = /usr/share/emacs
PKG=x
SITELISP = $(SHARE)/site-lisp/$(PKG)-$(VERSION)
SITESTART = $(SHARE)/site-lisp/site-start.d
AUTOLOADS=$(PKG)-autoloads
EMACS:=$(EMACSPATH)/emacs

PWD=$(shell pwd)
MAKE_AUTOLOADS=../bin/make-autoloads

XZFLAGS = -t1

.PHONY: FORCE

# everything in this directory is shippable
SOURCES=*.el
SHIPS=$(SOURCES)
TARGETS := $(SOURCES:.el=.elc)

all:

sources:
	@echo $(SOURCES)

ship: compile autoloads
	rm -rf $(SITELISP)
	mkdir -p $(SITELISP)
	install -m 444 $(TARGETS) $(SITELISP)
	mkdir -p $(SITESTART)
	install -m 444 $(AUTOLOADS) $(SITESTART)

autoloads: FORCE
	$(MAKE_AUTOLOADS) --top $(SITELISP) -c $(SOURCES) --sourcepath $(PWD) > $(AUTOLOADS)

.xz.dat: $(SOURCES)
	xz $(XZFLAGS) $^

clean: FORCE
	rm -f $(AUTOLOADS) *.elc .xz.dat

compile: clean
	$(EMACS) --batch --load=byte-compile-directory.el -e batch-byte-compile $(SHIPS)
