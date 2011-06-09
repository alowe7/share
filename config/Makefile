# $Id: Makefile 77 2011-04-29 23:19:54Z alowe $

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

ship: compile config

config: FORCE
	mkdir -p $(SITELISP)
	install -m 444 $(CONFIGTARGET) $(SITELISP)

clean: FORCE
	rm -f $(AUTOLOADS) *.elc 

compile:
ifeq ($(strip $(EMACS)),)
	@echo "EMACS not found"
else
	$(EMACS) --batch -e batch-byte-compile $(CONFIG)
endif
