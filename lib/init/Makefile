#############################################################################
#
# MODULE:   	GRASS Initialization
# AUTHOR(S):	Original author unknown - probably CERL
#   	    	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
#               Markus Neteler, Andrea Antonello
# PURPOSE:  	To create the various scripts and programs which are required
#   	    	to start GRASS.
# COPYRIGHT:    (C) 2000 by the GRASS Development Team
#
#               This program is free software under the GNU General Public
#   	    	License (>=v2). Read the file COPYING that comes with GRASS
#   	    	for details.
#
#############################################################################

MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make


START_UP=grass$(GRASS_VERSION_MAJOR)$(GRASS_VERSION_MINOR)

EXTRA_CFLAGS = -DD_LOCATION_NAME=\"$(DEFAULT_LOCATION)\" -DD_GISDBASE=\"$(DEFAULT_DATABASE)\" -DGRASS_VERSION_NUMBER=\"'$(GRASS_VERSION_NUMBER)'\" -DGRASS_VERSION_UPDATE_PKG=\"'$(GRASS_VERSION_UPDATE_PKG)'\"
SET_DATA = $(OBJDIR)/set_data.o $(OBJDIR)/mke_mapset.o \
	   $(OBJDIR)/mke_loc.o $(OBJDIR)/chk_dbase.o $(OBJDIR)/other.o

ifdef USE_CURSES
SET_DATA_PROG = $(ETC)/set_data$(EXE_SUFFIX)
else
SET_DATA_PROG =
endif

default: $(ARCH_BINDIR)/$(START_UP) \
	$(ETC)/Init.sh \
	$(SET_DATA_PROG) \
	$(ETC)/clean_temp$(EXE_SUFFIX) \
	$(ETC)/lock$(EXE_SUFFIX) \
	$(ETC)/run$(EXE_SUFFIX) \
	$(ETC)/echo$(EXE_SUFFIX) \
	$(ETC)/grass_intro \
	$(ETC)/license \
	$(ETC)/welcome \
	$(ETC)/VERSIONNUMBER \
	$(ETC)/msgs \
	$(ETC)/gis_set.tcl \
	$(ETC)/epsg_option.tcl \
	$(ETC)/file_option.tcl \
	$(ETC)/gintro.gif \
	$(ETC)/help.tcl \
	$(ETC)/make_location_epsg.sh \
	$(ETC)/grass-run.sh
	$(MKDIR) $(GISBASE)/docs/html
	$(INSTALL) -m 644 variables.html $(GISBASE)/docs/html/variables.html
	$(INSTALL) -m 644 grass6.html $(GISBASE)/docs/html/grass6.html
	$(INSTALL) -m 644 helptext.html $(GISBASE)/docs/html/helptext.html


$(ARCH_BINDIR)/$(START_UP): init.sh 
	@test -d $(ARCH_BINDIR) || (echo 'ARCH_DISTDIR($(ARCH_BINDIR))' not found; exit 1)
	@test -w $(ARCH_BINDIR) || (echo '($(ARCH_BINDIR))' not writeable; exit 1)
	rm -f $(ARCH_BINDIR)/$(START_UP) ; true
	$(SHELL) -c "sed \
	-e \"s#GISBASE_VALUE#$(GISBASE)#\" \
	grass.src > $(ARCH_BINDIR)/$(START_UP) 2>/dev/null ; true"
	chmod a+x $(ARCH_BINDIR)/$(START_UP)

$(ETC)/Init.sh: init.sh
	rm -f $@
	$(SHELL) -c "sed \
	-e \"s#GRASS_VERSION_NUMBER#$(GRASS_VERSION_NUMBER)#\" \
	-e \"s#GRASS_VERSION_DATE#$(GRASS_VERSION_DATE)#\" \
	-e \"s#GRASS_VERSION_UPDATE_PKG#$(GRASS_VERSION_UPDATE)#\" \
	-e \"s#LD_LIBRARY_PATH_VAR#$(LD_LIBRARY_PATH_VAR)#g\" \
	-e \"s#PERL_COMMAND#$(PERL)#\" \
	-e \"s#START_UP#$(START_UP)#\" \
	init.sh > $@"
	chmod +x $@

$(ETC)/set_data$(EXE_SUFFIX): $(SET_DATA)
	$(CC) $(LDFLAGS) -o $@ $(SET_DATA) $(EDITLIB) $(GISLIB) $(DATETIMELIB) $(VASKLIB) $(CURSES) $(MATHLIB) $(XDRLIB)

$(ETC)/echo$(EXE_SUFFIX): $(OBJDIR)/echo.o
	$(CC) $(LDFLAGS) $(OBJDIR)/echo.o -o $@

$(ETC)/clean_temp$(EXE_SUFFIX): $(OBJDIR)/clean_temp.o  
	$(CC) $(LDFLAGS) $(OBJDIR)/clean_temp.o $(GISLIB) $(DATETIMELIB) $(MATHLIB) $(XDRLIB) -o $@

$(ETC)/run$(EXE_SUFFIX): $(OBJDIR)/run.o
	$(CC) $(LDFLAGS) $(OBJDIR)/run.o -o $@

$(ETC)/lock$(EXE_SUFFIX): $(OBJDIR)/lock.o
	$(CC) $(LDFLAGS) $(OBJDIR)/lock.o $(GISLIB) $(DATETIMELIB) $(MATHLIB) $(XDRLIB) -o $@

$(BIN)/exit:
	touch $@
	chmod 600 $@

$(ETC)/VERSIONNUMBER:
	rm -f $@
	echo "$(GRASS_VERSION_NUMBER)" > $@
	chmod +r $@

$(ETC)/grass_intro: grass_intro.txt version.sed 
	rm -f $@
	sh ./version.sed "$(GRASS_VERSION_NUMBER)" "$(GRASS_VERSION_DATE)" "$(GRASS_VERSION_UPDATE_PKG)" grass_intro.txt > $@
	chmod +r $@

$(ETC)/license: license.txt version.sed
	rm -f $@
	sh ./version.sed "$(GRASS_VERSION_NUMBER)" "$(GRASS_VERSION_DATE)" "$(GRASS_VERSION_UPDATE_PKG)" license.txt > $@
	chmod +r $@

$(ETC)/welcome: welcome.txt version.sed
	rm -f $@
	sh ./version.sed "$(GRASS_VERSION_NUMBER)" "$(GRASS_VERSION_DATE)" "$(GRASS_VERSION_UPDATE_PKG)" welcome.txt > $@
	chmod +r $@

$(ETC)/msgs: msgs
	rm -rf $@
	$(MKDIR) $@
	$(INSTALL) -m 644 msgs/*.msg $@	

$(ETC)/gintro.gif: gintro.gif
	rm -f $@
	$(INSTALL) -m 644 gintro.gif $@

$(ETC)/gis_set.tcl: gis_set.tcl
	rm -f $@
	$(INSTALL) -m 755 gis_set.tcl $(ETC)

$(ETC)/epsg_option.tcl: epsg_option.tcl.in
	rm -f $@
	sh ./projshare.sed "$(PROJSHARE)" epsg_option.tcl.in > $@
	chmod 755 $@

$(ETC)/help.tcl: help.tcl
	rm -f $@
	$(INSTALL) -m 755 help.tcl $(ETC)

$(ETC)/file_option.tcl: file_option.tcl
	rm -f $@
	$(INSTALL) -m 755 file_option.tcl $(ETC)

$(ETC)/make_location_epsg.sh: make_location_epsg.sh.in
	rm -f $@
	sh ./projshare.sed "$(PROJSHARE)" make_location_epsg.sh.in > $@
	chmod 755 $@

$(ETC)/grass-run.sh: grass-run.src
	rm -f $@
	$(SHELL) -c "sed \
	-e \"s#LD_LIBRARY_PATH_VAR#$(LD_LIBRARY_PATH_VAR)#g\" \
	grass-run.src > $@"
	chmod +x $@
