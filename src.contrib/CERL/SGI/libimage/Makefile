#!smake

include $(ROOT)/usr/include/make/commondefs

LCOPTS	= -xansi -D__STDC__
LCDEFS	= -O 
CFILES	= close.c filbuf.c flsbuf.c name.c open.c pix.c rdwr.c rle.c row.c
CVERSION=

TARGETS = lib/libimage.a

default: $(TARGETS)

include	$(COMMONRULES)

$(TARGETS): $(OBJECTS)
	$(AR) scr $(TARGETS) $(OBJECTS)
	rm *.o

