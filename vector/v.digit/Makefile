
MODULE_TOPDIR = ../..

PGM=v.digit
TBOX=$(GISBASE)/etc/v.digit/toolbox.tcl
SET=$(GISBASE)/etc/v.digit/settings.tcl

LIBES     = $(D_LIB) $(DISPLAYLIB) $(RASTERLIB) $(FORMLIB) $(VECTLIB) $(VECTLIB_REAL) $(GISLIB)
DEPENDENCIES= $(D_DEP) $(DISPLAYDEP) $(RASTERDEP) $(VECTDEP) $(GISDEP)

EXTRA_CFLAGS = $(TCLINCDIR) $(TKINCDIR)
XTRA_LDFLAGS=$(DFLAGS) $(TCLTKLIBPATH) $(TCLTKLIBS)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd $(TBOX) $(SET) ico

$(TBOX): toolbox.tcl
	if [ ! -d $(GISBASE)/etc ]; then mkdir $(GISBASE)/etc; fi
	if [ ! -d $(GISBASE)/etc/v.digit ]; then mkdir $(GISBASE)/etc/v.digit; fi
	cp toolbox.tcl $(TBOX)
	chmod 0755 $(TBOX)

$(SET): settings.tcl
	cp settings.tcl $(SET)
	chmod 0755 $(SET)

ico:
	cp icons/*.gif $(GISBASE)/etc/v.digit/
