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
	if [ ! -d $(ETC)/wxpython ] ; then $(MKDIR) $(ETC)/wxpython ; fi
	if [ ! -d $(ETC)/wxpython/scripts ] ; then $(MKDIR) $(ETC)/wxpython/scripts ; fi
	for file in p.* ; do \
	sed -e "s#SCRIPT_NAME#$$file#" $(MODULE_TOPDIR)/gui/scripts/windows_launch.bat \
		| sed -e "s#etc/gui/#etc/wxpython/#" \
		> $(ETC)/wxpython/scripts/$$file.bat ; done

clean: cleansubdirs
