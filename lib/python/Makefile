MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make
include $(MODULE_TOPDIR)/include/Make/Doxygen.make

default: $(ETC)/python

$(ETC)/python: grass.py README.txt
	if [ ! -d $@ ]; then $(MKDIR) $@; fi
	for file in $^ ; do $(INSTALL_DATA) $$file $@ ; done

.PHONY: $(ETC)/python

clean:

#doxygen:
DOXNAME=grasspython
