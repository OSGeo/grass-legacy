
MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

default: $(ETC)/gtcltk

$(ETC)/gtcltk: *.tcl 
	if [ ! -d $(ETC)/gtcltk ]; then mkdir $(ETC)/gtcltk; fi
	cp *.tcl $(ETC)/gtcltk/
