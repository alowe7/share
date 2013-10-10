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

EMACS := $(shell which emacs  2> /dev/null)

PWD=$(shell pwd)

XZFLAGS = -t1

.PHONY: FORCE

# almost everything in this directory is shippable
SOURCES:=$(shell find  . -name "*.el" -a ! -name "byte-compile-directory.el")

TARGETS := $(SOURCES:.el=.elc)

all:

sources:
	@echo $(SOURCES)

ship: compile autoloads pkglisp

pkglisp: FORCE
	rm -rf $(PKGLISP)
	install -d $(PKGLISP)
	install -m 444 $(TARGETS) $(PKGLISP)
	install -d $(SITESTART)
	install -m 444 $(AUTOLOADS) $(SITESTART)

autoloads: FORCE
ifeq ($(strip $(EMACS)),)
	@echo "EMACS not found"
else
	$(EMACS) -batch --directory "/usr/share/emacs/site-lisp" --load="make-autoloads" --eval "(make-autoloads  \"$(PWD)\" \"$(PKGLISP)\" \"$(PKG)\" t t)"
endif

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
