MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Dir.make

ETCDIR = $(ETC)/gui/scripts

default: install_scripts

ifdef MINGW
SCRIPT_ACTIONS += create.bat
endif

install_scripts: $(SCRIPT_ACTIONS)
	$(MKDIR) $(ETCDIR)
	$(INSTALL) d.rast3d $(ETCDIR)/

create.bat:
	if [ ! -d $(ETC)/gui ] ; then $(MKDIR) $(ETC)/gui ; fi
	if [ ! -d $(ETCDIR) ] ; then $(MKDIR) $(ETCDIR) ; fi
	sed -e "s#SCRIPT_NAME#d.rast3d#" $(MODULE_TOPDIR)/gui/scripts/windows_launch.bat \
		> $(ETCDIR)/d.rast3d.bat

clean: cleansubdirs
