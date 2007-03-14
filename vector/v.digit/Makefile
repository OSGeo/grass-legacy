MODULE_TOPDIR = ../..

PGM = v.digit

LIBES = $(DISPLAYLIB) $(RASTERLIB) $(VECTLIB) $(GISLIB) $(TCLTKLIBPATH) $(TCLTKLIBS)
DEPENDENCIES= $(DISPLAYDEP) $(RASTERDEP) $(VECTDEP) $(GISDEP)

EXTRA_CFLAGS = $(XCFLAGS) $(TCLINCDIR) $(TKINCDIR) $(VECT_CFLAGS)
XTRA_LDFLAGS=$(DFLAGS) 
EXTRA_INC = $(VECT_INC)

include $(MODULE_TOPDIR)/include/Make/Module.make

AUXDIR := $(GISBASE)/etc/v.digit
TCLFILES := $(patsubst %,$(AUXDIR)/%,$(wildcard *.tcl))

default: cmd $(TCLFILES) ico

$(AUXDIR):
	if [ ! -d $(ETC) ]; then $(MKDIR) $(ETC); fi
	if [ ! -d $(AUXDIR) ]; then $(MKDIR) $@; fi

$(AUXDIR)/%.tcl: %.tcl $(AUXDIR)
	$(INSTALL_DATA) $< $@

ico:
	$(INSTALL_DATA) icons/*.gif $(AUXDIR)
	$(INSTALL_DATA) icons/*.xbm $(AUXDIR)
