#############################################################################
#
# $Id$
#
# MODULE:   	Grass Initialization
# AUTHOR(S):	Original author unknown - probably CERL
#   	    	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
# PURPOSE:  	To create the various scripts and programs which are required
#   	    	to start grass.
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


START_UP=grass$(VERSION_NAME)

EXTRA_CFLAGS = -DD_LOCATION_NAME=\"$(DEFAULT_LOCATION)\" -DD_GISDBASE=\"$(DEFAULT_DATABASE)\" -DVERSION_NUMBER=\"'$(VERSION_NUMBER)'\" -DVERSION_UPDATE_PKG=\"'$(VERSION_UPDATE_PKG)'\"
SET_DATA = $(OBJDIR)/set_data.o $(OBJDIR)/mke_mapset.o \
	   $(OBJDIR)/mke_loc.o $(OBJDIR)/chk_dbase.o $(OBJDIR)/other.o

default: $(ARCH_BINDIR)/$(START_UP) \
     $(ETC)/Init.sh \
     $(ETC)/set_data \
     $(ETC)/clean_temp \
     $(ETC)/lock \
     $(ETC)/run \
     $(ETC)/echo \
     $(ETC)/grass_intro

$(ARCH_BINDIR)/$(START_UP): init.sh 
	@test -d $(ARCH_BINDIR) || (echo 'ARCH_DISTDIR($(ARCH_BINDIR))' not found; exit 1)
	@test -w $(ARCH_BINDIR) || (echo '($(ARCH_BINDIR))' not writeable; exit 1)
	rm -f $(ARCH_BINDIR)/$(START_UP) ; true
	$(SHELL) -c "sed -e \"s#GISBASE_VALUE#$(GISBASE)#\" -e \"s#PERL_COMMAND#$(PERL)#\" grass.src > $(ARCH_BINDIR)/$(START_UP) 2>/dev/null ; true"
	chmod a+x $(ARCH_BINDIR)/$(START_UP)

$(ETC)/Init.sh: init.sh version.sed 
	rm -f $@
	sh ./version.sed "$(VERSION_NUMBER)" "$(VERSION_DATE)" "$(VERSION_UPDATE_PKG)" init.sh > $@
	chmod +x $@

$(ETC)/set_data: $(SET_DATA)
	$(CC) $(LDFLAGS) -o $@ $(SET_DATA) $(EDITLIB) $(GISLIB) $(VASKLIB) $(CURSES) $(MATHLIB) $(XDRLIB)

$(ETC)/echo: $(OBJDIR)/echo.o
	$(CC) $(LDFLAGS) $(OBJDIR)/echo.o -o $@

$(ETC)/clean_temp: $(OBJDIR)/clean_temp.o  
	$(CC) $(LDFLAGS) $(OBJDIR)/clean_temp.o $(GISLIB) $(MATHLIB) $(XDRLIB) -o $@

$(ETC)/run: $(OBJDIR)/run.o
	$(CC) $(LDFLAGS) $(OBJDIR)/run.o -o $@

$(ETC)/lock: $(OBJDIR)/lock.o
	$(CC) $(LDFLAGS) $(OBJDIR)/lock.o $(GISLIB) -o $@

$(BIN)/exit:
	touch $@
	chmod 600 $@

$(ETC)/grass_intro: grass_intro.txt version.sed 
	rm -f $@
	sh ./version.sed "$(VERSION_NUMBER)" "$(VERSION_DATE)" "$(VERSION_UPDATE_PKG)" grass_intro.txt > $@
	chmod +r $@

