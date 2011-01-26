# $Id$

VERSION = 1.0

SHELL=/bin/sh
.PHONY: FORCE

all:

ship:
	$(MAKE) --directory bin ship
	$(MAKE) --directory site-lisp ship

test: FORCE
	$(MAKE) --directory site-lisp test



