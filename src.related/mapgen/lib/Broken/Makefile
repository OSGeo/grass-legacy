# "@(#)Makefile	AMG v.1.3";
# this library contains missing or corrections for various tools
# normally expected in the host UNIX library.
#
# Installers should check for the existance and viability of their
# versions of the tools listed:

# hypot (math lib) : Many distributions of UNIX V, error hypot(0,0)
# log	(math lib) : Microport System V/AT 2.2, error .75 <= arg < 1.
# strtol (libc) : ANSI routine missing from BSD
# strtod (libc) : ANSI routine missing from BSD
# fmod	(math lib (?)) : ANSI routine missing from BSD
# tempnam : version V emmulation for BSD (can't find any equivalencies
#		for this operation in BSD)

# installer should check each individual subsystem Makefile for
# requirements of this library.

CFLAGS = -O -I$(INCL)/src $(MODE)

install:
	make MODE= LIB=../libbroke.a lib
	make MODE=-Ml LIB=../l_libbroke.a lib # uPort or XENIX only

lib:	$(LIB)(hypot.o) $(LIB)(log.o)
	$(RANLIB) $(LIB)

