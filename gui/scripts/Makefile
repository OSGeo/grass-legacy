MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Dir.make

ifdef MINGW
SCRIPT_ACTIONS += create.bat
endif

default: $(SCRIPT_ACTIONS)
	if [ ! -d $(ETC)/gui ] ; then $(MKDIR) $(ETC)/gui ; fi
	if [ ! -d $(ETC)/gui/scripts ] ; then $(MKDIR) $(ETC)/gui/scripts ; fi
	for file in d.* g.* r.* v.* ; do $(INSTALL) $$file $(ETC)/gui/scripts/ ; done

create.bat:
	if [ ! -d $(ETC)/gui ] ; then $(MKDIR) $(ETC)/gui ; fi
	if [ ! -d $(ETC)/gui/scripts ] ; then $(MKDIR) $(ETC)/gui/scripts ; fi
	for file in d.* g.*sh r.* v.*sh ; do \
	sed -e "s#SCRIPT_NAME#$$file#" $(MODULE_TOPDIR)/gui/scripts/windows_launch.bat \
		> $(ETC)/gui/scripts/$$file.bat ; \
	sed -e "s#SCRIPT_NAME#$$file#" $(MODULE_TOPDIR)/gui/scripts/windows_launch.bat \
		> $(BIN)/$$file.bat ; done
