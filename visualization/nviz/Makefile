# Toplevel makefile for building everything

MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

VER=2.2
NV_VER=nviz$(VER)
PROG=nviz
NVWISH_VER=NVWISH$(VER)
INSTALL_DIR=$(GISBASE)/etc/$(NV_VER)

SCRIPTDIR=$(NV_VER)/scripts

default: nvwish nviz install 

#
# DO NOT CHANGE configure: TARGET TO doconfigure:
# SOME make WON'T WORK IF TARGET AND SHELL NAME ARE EQUAL.
#

nvwish:
	cd src ; make

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
	$(INSTALL) scripts/*Index $(INSTALL_DIR)/scripts
	(cd bitmaps; tar -cf - *  | (cd $(INSTALL_DIR)/bitmaps; tar  -xf - 2>/dev/null ); true)
	$(INSTALL) scripts/$(NV_VER)_script $(INSTALL_DIR)/scripts
	$(INSTALL) src/$(PROG) $(GISBASE)/bin
	(cd html ; make )

clean: 
	(cd src && $(MAKE) clean) || exit 1;

