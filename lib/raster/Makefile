MODULE_TOPDIR = ../..

PACKAGE ="libraster"
DEFS=-DPACKAGE=\"$(PACKAGE)\"
EXTRA_CFLAGS=$(GETHOSTNAME) $(ZLIBINCPATH) $(PICFLAGS) $(DEFS)

EXTRA_LIBS=$(GISLIB)
LIB_NAME = $(RASTER_LIBNAME)

LIB_OBJS = commands.o get.o io.o io_sock.o io_fifo.o pad.o parse_mon.o protocol.o

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib

