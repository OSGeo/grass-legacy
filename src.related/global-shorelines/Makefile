#  The DFLIP flag is needed for some architectures only - 
#  it is necessary for Digital Unix
#  it should be removed for Irix

CFLAGS=-DFLIP

all:	gshhs gshhstograss

gshhstograss:	gshhstograss.c gshhs.h
	cc -o gshhstograss $(CFLAGS) gshhstograss.c

gshhs:	gshhs.c gshhs.h
	cc -o gshhs $(CFLAGS) gshhs.c

