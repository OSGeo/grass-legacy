
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
	if [ ! -d $(GISBASE)/etc ]; then mkdir $(GISBASE)/etc; fi
	if [ ! -d $(GISBASE)/etc/v.digit ]; then mkdir $(GISBASE)/etc/v.digit; fi
	cp toolbox.tcl $(TBOX)
	chmod 0755 $(TBOX)

$(SET): settings.tcl
	cp settings.tcl $(SET)
	chmod 0755 $(SET)

$(CAT): cats.tcl
	cp cats.tcl $(CAT)
	chmod 0755 $(CAT)

ico:
	cp icons/*.gif $(GISBASE)/etc/v.digit/
