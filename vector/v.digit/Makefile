MODULE_TOPDIR = ../..

PGM=v.digit
TBOX=$(GISBASE)/etc/v.digit/toolbox.tcl
SET=$(GISBASE)/etc/v.digit/settings.tcl
CAT=$(GISBASE)/etc/v.digit/cats.tcl

LIBES     = $(D_LIB) $(DISPLAYLIB) $(RASTERLIB) $(VECTLIB) $(FORMLIB) \
$(GISLIB) $(TCLTKLIBPATH) $(TCLTKLIBS)
DEPENDENCIES= $(D_DEP) $(DISPLAYDEP) $(RASTERDEP) $(VECTDEP) $(FORMDEP) $(GISDEP)

EXTRA_CFLAGS = $(XCFLAGS) $(TCLINCDIR) $(TKINCDIR) $(VECT_CFLAGS)
XTRA_LDFLAGS=$(DFLAGS) 
EXTRA_INC = $(VECT_INC)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd $(TBOX) $(SET) $(CAT) ico

$(TBOX): toolbox.tcl
	if [ ! -d $(GISBASE)/etc ]; then $(MKDIR) $(GISBASE)/etc; fi
	if [ ! -d $(GISBASE)/etc/v.digit ]; then $(MKDIR) $(GISBASE)/etc/v.digit; fi
	$(INSTALL) -m 755 toolbox.tcl $(TBOX)

$(SET): settings.tcl
	$(INSTALL) -m 755 settings.tcl $(SET)

$(CAT): cats.tcl
	$(INSTALL) -m 755 cats.tcl $(CAT)

ico:
	$(INSTALL) -m 644 icons/*.gif $(GISBASE)/etc/v.digit/
