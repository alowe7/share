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
EMACS:=$(EMACSPATH)/emacs

PWD=$(shell pwd)
MAKE_AUTOLOADS=../bin/make-autoloads

XZFLAGS = -t1

.PHONY: FORCE

# everything in this directory is shippable
CONFIG=config.el
SOURCES=assoc-helpers.el buff.el buffers.el byte-compile-directory.el cat-seq.el cat-utils.el collect-directories.el completion.el ctl-backslash.el ctl-comma.el ctl-meta.el ctl-ret.el ctl-slash.el ctl-x-ctl-a.el dom.el eval-process.el fapropos.el fb.el filetime.el fixframe.el indicate.el input.el long-comment.el mktime.el nums.el other.el perl-command.el php-mode.el pod.el qsave.el roll.el scratch-mode.el sh.el string-utils.el svn-helpers.el syntax.el trim.el typesafe.el whence.el xa.el xpath-helpers.el xpath-parser.el xpath.el zap.el

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
	$(MAKE_AUTOLOADS) --top $(PKGLISP) -c $(SOURCES) --sourcepath $(PWD) > $(AUTOLOADS)

.xz.dat: $(SOURCES)
	xz $(XZFLAGS) $^

clean: FORCE
	rm -f $(AUTOLOADS) *.elc .xz.dat

compile: clean
	$(EMACS) --batch --load=byte-compile-directory.el -e batch-byte-compile $(CONFIG) $(SOURCES)

test: compile
	$(MAKE) --directory ../test test