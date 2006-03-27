MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

GRASS_NVIZ_DOCS=$(GISBASE)/docs/html/nviz

default:
	if [ ! -d $(GISBASE)/docs ] ; then mkdir $(GISBASE)/docs; fi
	if [ ! -d $(GRASS_NVIZ_DOCS) ] ; then mkdir $(GRASS_NVIZ_DOCS); fi
#fix for CVS and Gmakefile:
	(tar -cf - *  | (cd $(GRASS_NVIZ_DOCS); tar  -xf - 2>/dev/null ); true)
