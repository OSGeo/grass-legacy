MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Dir.make

ifdef MINGW
SCRIPT_ACTIONS += create.bat
endif

default: $(SCRIPT_ACTIONS)
	if [ ! -d $(GISBASE)/etc/gui ] ; then $(MKDIR) $(GISBASE)/etc/gui ; fi
	if [ ! -d $(GISBASE)/etc/gui/scripts ] ; then $(MKDIR) $(GISBASE)/etc/gui/scripts ; fi
	for file in d.* g.* r.* v.* ; do $(INSTALL) $$file $(GISBASE)/etc/gui/scripts/ ; done

create.bat:
	for file in d.* g.* r.* v.* ; do \
	sed -e "s#SCRIPT_NAME#$$file#" $(MODULE_TOPDIR)/gui/scripts/windows_launch.bat \
		> $(GISBASE)/etc/gui/scripts/$$file.bat ; done
