MODULE_TOPDIR = ../..

EXTRA_LIBS=$(GISLIB)

LIB_NAME = $(SYMB_LIBNAME)

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib symb

symb:
	mkdir -p $(ETC)/symbol
	find ./symbol -type f | grep -v CVS | xargs tar cBf - |(cd $(ETC); tar xBf -)
