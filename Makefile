# $Id$

VERSION = 1.0

SHELL=/bin/sh
.PHONY: FORCE

all:

ship:
	$(MAKE) --directory site-lisp ship

