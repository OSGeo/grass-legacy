
MODULE_TOPDIR = ../..

LIB_NAME = $(SYMB_LIBNAME)

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib symb

symb:
	mkdir -p $(ETC)/symbol
	(cd symbol/ ; tar cBf - * --exclude=CVS | (cd $(ETC)/symbol/; tar xBf - ) )
