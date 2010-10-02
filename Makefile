# $Id: Makefile,v 1.1 2010-10-02 21:35:16 keystone Exp $

VERSION = 1.0

SHELL=/bin/sh
.PHONY: FORCE

all:

ship:
	$(MAKE) --directory site-lisp ship

