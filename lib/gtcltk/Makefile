MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

default: $(ETC)/gtcltk

$(ETC)/gtcltk: *.tcl 
	if [ ! -d $(ETC)/gtcltk ]; then $(MKDIR) $(ETC)/gtcltk; fi
	for file in *.tcl ; do $(INSTALL_DATA) $$file $(ETC)/gtcltk/ ; done

clean:
