MODULE_TOPDIR = ../..

PACKAGE ="libraster"
DEFS=-DPACKAGE=\"$(PACKAGE)\"
EXTRA_CFLAGS=$(GETHOSTNAME) $(FTINC) -I../pngdriver -I../driver $(PICFLAGS) $(DEFS)

EXTRA_LIBS=$(DRIVERLIB) $(GISLIB) $(FTLIB) $(ICONVLIB) $(PNGDRIVERLIB)
LIB_NAME = $(RASTER_LIBNAME)

LIB_OBJS =	\
	com_get.o		\
	com_io.o		\
	com_pad.o		\
	com_proto.o		\
	common.o		\
	io_fifo.o		\
	io_sock.o		\
	loc_get.o		\
	loc_io.o		\
	loc_pad.o		\
	loc_proto.o		\
	parse_mon.o		\
	rem_get.o		\
	rem_io.o		\
	rem_pad.o		\
	rem_proto.o		

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib
