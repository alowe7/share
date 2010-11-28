# $Id$

VERSION = 1.0

SHELL=/bin/sh
INSTALL = install
SHARE = /usr/share/emacs
PKG=x
SITELISP = $(SHARE)/site-lisp/$(PKG)-$(VERSION)
SITESTART = $(SHARE)/site-lisp/site-start.d
AUTOLOADS=$(PKG)-autoloads
# EMACS=$(EMACSPATH)/emacs
EMACS=/usr/local/lib/emacs-23.2/bin/emacs
PWD=$(shell pwd)
MAKE_AUTOLOADS  := $(shell which make-autoloads  2> /dev/null)

XZFLAGS = -t1

.PHONY: FORCE

# everything in this directory is shippable
SOURCES=*.el
SHIPS=$(SOURCES)
TARGETS := $(SOURCES:.el=.elc)

all:

sources:
	@echo $(SOURCES)

ship: compile
	rm -rf $(SITELISP)
	mkdir -p $(SITELISP)
	install -m 444 $(TARGETS) $(SITELISP)
	$(MAKE_AUTOLOADS) --top $(SITELISP) -c $(SOURCES) > $(AUTOLOADS)
	mkdir -p $(SITESTART)
	install -m 444 $(AUTOLOADS) $(SITESTART)

.xz.dat: $(SOURCES)
	xz $(XZFLAGS) $^

clean: $(FORCE)
	rm -f $(AUTOLOADS) *.elc

compile: $(SHIPS)
	$(EMACS) --batch --load=byte-compile-directory.el -e batch-byte-compile $^
