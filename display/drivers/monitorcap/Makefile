
MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

default: $(ETC)/monitorcap
	
$(ETC)/monitorcap: monitorcap  
	sed -e s:BASEDIR:$(GISBASE):g monitorcap > $(ETC)/monitorcap
