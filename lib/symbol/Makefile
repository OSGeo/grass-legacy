
MODULE_TOPDIR = ../..

LIB_NAME = $(SYMB_LIBNAME)

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib symb

symb:
	mkdir -p $(GISBASE)/etc/symbol
	cp -r symbol/* $(GISBASE)/etc/symbol/ 
