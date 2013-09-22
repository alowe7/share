# $Id$

VERSION = 1.0

SHELL=/bin/sh
INSTALL = install
SHARE = /usr/share/emacs
SITELISP = $(SHARE)/site-lisp
SITESTART = $(SITELISP)/site-start.d
PWD=$(shell pwd)

EMACS := $(shell which emacs  2> /dev/null)

.PHONY: FORCE

# everything in this directory is shippable
CONFIGS=config.el make-autoloads.el
CONFIGINIT=config-init.el

CONFIGTARGETS := $(CONFIGS:.el=.elc)

all:

ship: compile init
	mkdir -p $(SITELISP)
	install -m 444 $(CONFIGTARGETS) $(SITELISP)
	mkdir -p $(SITESTART)
	install -m 444 $(CONFIGINIT) $(SITESTART)

init: FORCE
	@echo "(add-to-list 'find-function-source-path \"$(PWD)\")" > $(CONFIGINIT)

clean: FORCE
	rm -f *.elc config-init.el

compile: FORCE
ifeq ($(strip $(EMACS)),)
	@echo "EMACS not found"
else
	$(EMACS) --batch -e batch-byte-compile $(CONFIGS)
endif
