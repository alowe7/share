# $Id: Makefile 77 2011-04-29 23:19:54Z alowe $

VERSION = 1.0

SHELL=/bin/sh
INSTALL = install
SHARE = /usr/share/emacs
SITELISP = $(SHARE)/site-lisp
SITESTART = $(SITELISP)/site-start.d
PWD=$(shell pwd)

ifneq ($(strip $(EMACSPATH)),)
EMACS := $(EMACSPATH)/emacs
else
EMACS := $(shell which emacs  2> /dev/null)
endif

.PHONY: FORCE

# everything in this directory is shippable
CONFIG=config.el
CONFIGINIT=config-init.el

CONFIGTARGET := $(CONFIG:.el=.elc)

all:

ship: compile init
	mkdir -p $(SITELISP)
	install -m 444 $(CONFIGTARGET) $(SITELISP)
	install -m 444 $(CONFIGINIT) $(SITESTART)

init: FORCE
	@echo "(add-to-list 'find-function-source-path \"$(PWD)\")" > $(CONFIGINIT)

clean: FORCE
	rm -f *.elc config-init.el

compile: FORCE
ifeq ($(strip $(EMACS)),)
	@echo "EMACS not found"
else
	$(EMACS) --batch -e batch-byte-compile $(CONFIG)
endif
