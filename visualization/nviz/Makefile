# Toplevel makefile for building everything

MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

VER=2.2
NV_VER=nviz$(VER)
PROG=nviz
NVWISH_VER=$(PROG)$(EXE)
INSTALL_DIR=$(GISBASE)/etc/$(NV_VER)

SCRIPTDIR=$(NV_VER)/scripts

default: nvwish nviz install docs

#
# DO NOT CHANGE configure: TARGET TO doconfigure:
# SOME make WON'T WORK IF TARGET AND SHELL NAME ARE EQUAL.
#

nvwish:
	cd src ; $(MAKE)

nviz:
	'rm' -rf $(INSTALL_DIR)
	$(MKDIR) $(INSTALL_DIR)

# Install the scripts in the appropriate directory
install:
	'rm' -rf $(INSTALL_DIR)/
	$(MKDIR) $(INSTALL_DIR)
	$(MKDIR) $(INSTALL_DIR)/scripts
	$(MKDIR) $(INSTALL_DIR)/bitmaps
	(cd scripts; $(INSTALL) script_get_line script_play script_tools script_file_tools $(INSTALL_DIR)/scripts)
	$(INSTALL) scripts/*.tcl $(INSTALL_DIR)/scripts
### ACS - BEGIN ###############################################################
	$(INSTALL) scripts/nviz_params $(INSTALL_DIR)/scripts/nviz_params
### ACS - END #################################################################
	$(INSTALL) scripts/*Index $(INSTALL_DIR)/scripts
	$(INSTALL) -m 755 scripts/$(PROG) $(GISBASE)/bin
	if test -f "src/nvwish.exe" ; then mv src/nvwish.exe $(INSTALL_DIR)/$(NVWISH_VER) ; fi
	if test -f "src/nvwish" ; then mv src/nvwish $(INSTALL_DIR)/$(NVWISH_VER) ; fi
	(cd bitmaps; tar -cf - *  | (cd $(INSTALL_DIR)/bitmaps; tar  -xf - 2>/dev/null ); true)
	$(INSTALL) scripts/$(NV_VER)_script $(INSTALL_DIR)/scripts
	$(INSTALL) scripts/nviz.bat $(GISBASE)/bin
	(cd html ; $(MAKE) )

clean: 
	(cd src && $(MAKE) clean) || exit 1;

docs:
	-$(MKDIR) $(ARCH_DISTDIR)/docs/html
	$(INSTALL_DATA) description.html $(ARCH_DISTDIR)/docs/html/$(PROG).html
