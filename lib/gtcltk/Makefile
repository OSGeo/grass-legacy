MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

default: $(ETC)/gtcltk

$(ETC)/gtcltk: *.tcl 
	if [ ! -d $(ETC)/gtcltk ]; then $(MKDIR) $(ETC)/gtcltk; fi
	$(INSTALL) -m 644 *.tcl $(ETC)/gtcltk/

clean:
