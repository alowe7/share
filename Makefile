# $Id$

VERSION = 1.0

SHELL=/bin/sh
.PHONY: FORCE

all:

ship:
	$(MAKE) --directory bin ship
	$(MAKE) --directory config ship
	$(MAKE) --directory site-lisp ship

compile:
	$(MAKE) --directory site-lisp compile

test: FORCE
	$(MAKE) --directory site-lisp test



