# $Id$

VERSION = 1.0

SHELL=/bin/sh
INSTALL = install
SHARE = /usr/share/emacs
PKG=x
SITELISP = $(SHARE)/site-lisp
PKGLISP = $(SITELISP)/$(PKG)-$(VERSION)
SITESTART = $(SITELISP)/site-start.d
AUTOLOADS=$(PKG)-autoloads

ifneq ($(strip $(EMACSPATH)),)
EMACS := $(EMACSPATH)/emacs
else
EMACS := $(shell which emacs  2> /dev/null)
endif


PWD=$(shell pwd)
MAKE_AUTOLOADS=../bin/make-autoloads

XZFLAGS = -t1

.PHONY: FORCE

# everything in this directory is shippable
CONFIG=config.el
SOURCES:=$(shell find . -name "*.el")

TARGETS := $(SOURCES:.el=.elc)

# config target needs to get installed above pkglisp
CONFIGTARGET := $(CONFIG:.el=.elc)

all:

sources:
	@echo $(SOURCES)

ship: compile autoloads
	install -m 444 $(CONFIGTARGET) $(SITELISP)
	rm -rf $(PKGLISP)
	mkdir -p $(PKGLISP)
	install -m 444 $(TARGETS) $(PKGLISP)
	mkdir -p $(SITESTART)
	install -m 444 $(AUTOLOADS) $(SITESTART)

autoloads: FORCE
	$(MAKE_AUTOLOADS) --top=$(PKGLISP) --prefix=$(PKG) -c $(SOURCES) --sourcepath $(PWD) > $(AUTOLOADS)

.xz.dat: FORCE
	xz $(XZFLAGS) $(SOURCES)

clean: FORCE
	rm -f $(AUTOLOADS) *.elc .xz.dat

compile:
ifeq ($(strip $(EMACS)),)
	@echo "EMACS not found"
else
	$(EMACS) --batch --load=byte-compile-directory.el -e batch-byte-compile $(CONFIG) $(SOURCES)
endif

test: compile
	$(MAKE) --directory ../test test