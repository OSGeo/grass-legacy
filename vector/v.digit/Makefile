
MODULE_TOPDIR = ../..

PGM=v.digit
TBOX=$(GISBASE)/etc/v.digit/toolbox.tcl

LIBES     = $(DISPLAYLIB) $(RASTERLIB) $(VECTLIB) $(VECTLIB_REAL) $(GISLIB)
EXTRA_CFLAGS = $(TCLINCDIR) $(TKINCDIR)
XTRA_LDFLAGS=$(DFLAGS) $(TCLTKLIBPATH) $(TCLTKLIBS)
 
include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd $(TBOX) icons

$(TBOX): toolbox.tcl
	if [ ! -d $(GISBASE)/etc ]; then mkdir $(GISBASE)/etc; fi
	if [ ! -d $(GISBASE)/etc/v.digit ]; then mkdir $(GISBASE)/etc/v.digit; fi
	cp toolbox.tcl $(TBOX)
	chmod 0755 $(TBOX)

icons:
	cp *.gif $(GISBASE)/etc/v.digit/
