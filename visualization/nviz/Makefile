# $Id$
#
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

default: configure nvwish nviz install 

#
# DO NOT CHANGE configure: TARGET TO doconfigure:
# SOME make WON'T WORK IF TARGET AND SHELL NAME ARE EQUAL.
#
configure:
	@GRASS_IN_GMAKEFILE=yes ./doconfigure $(VER) "$(PQLIB)"

nvwish:
	cd src ; make

nviz:
	'rm' -rf $(INSTALL_DIR)
	mkdir $(INSTALL_DIR)

# Install the scripts in the appropriate directory
install:
	'rm' -rf $(INSTALL_DIR)/
	mkdir $(INSTALL_DIR)
	mkdir $(INSTALL_DIR)/scripts
	mkdir $(INSTALL_DIR)/bitmaps
	(cd scripts; cp -L script_get_line script_play script_tools script_file_tools $(INSTALL_DIR)/scripts)
	cp -L scripts/*.tcl $(INSTALL_DIR)/scripts
	cp -L scripts/*Index $(INSTALL_DIR)/scripts
	(cd bitmaps; tar -cf - *  | (cd $(INSTALL_DIR)/bitmaps; tar  -xf - 2>/dev/null ); true)
	cp -L scripts/$(NV_VER)_script $(INSTALL_DIR)/scripts
	cp -L scripts/$(PROG) $(GISBASE)/bin
	chmod ugo+x $(INSTALL_DIR)/scripts/$(NV_VER)_script \
		$(INSTALL_DIR)/scripts/script_file_tools \
		$(INSTALL_DIR)/scripts/script_get_line \
		$(INSTALL_DIR)/scripts/script_play \
		$(INSTALL_DIR)/scripts/script_tools \
		$(GISBASE)/bin/$(PROG)
# test required for winGRASS, check for cygwin first:
	if test -f "src/nvwish.exe" ; then mv src/nvwish.exe $(INSTALL_DIR)/$(NVWISH_VER) ; fi
	if test -f "src/nvwish" ; then mv src/nvwish $(INSTALL_DIR)/$(NVWISH_VER) ; fi
#	$(GMAKE) html

clean: 
	(cd src && make clean) || exit 1;

