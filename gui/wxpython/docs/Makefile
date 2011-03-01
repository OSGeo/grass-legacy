MODULE_TOPDIR = ../../..

FILES := $(wildcard *.html)

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

default: $(patsubst %,$(HTMLDIR)/%,$(FILES))
