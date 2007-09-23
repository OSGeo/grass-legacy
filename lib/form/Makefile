MODULE_TOPDIR = ../..

EXTRA_LIBS=$(GISLIB) $(DBMILIB)

LIB_NAME = $(FORM_LIBNAME)

HTMLLIB=$(GISBASE)/etc/form/html_library.tcl
FORM=$(GISBASE)/etc/form/form.tcl
FORMPROG=$(GISBASE)/etc/form/form
EXTRA_CFLAGS = $(XCFLAGS) $(TCLINCDIR) $(TKINCDIR)

include $(MODULE_TOPDIR)/include/Make/Platform.make

LIB_OBJS = generate.o open.o
ifneq ($(TCLTKLIBS),)
    GTCLTKFORM = $(HTMLLIB) $(FORM) $(FORMPROG) 
endif

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib $(GTCLTKFORM)

$(HTMLLIB): html_library_grass.tcl
	if [ ! -d $(GISBASE)/etc ]; then $(MKDIR) $(GISBASE)/etc; fi
	if [ ! -d $(GISBASE)/etc/form ]; then $(MKDIR) $(GISBASE)/etc/form; fi
	$(INSTALL) -m 644 html_library_grass.tcl $(HTMLLIB)

$(FORM): form.tcl
	$(INSTALL) -m 644 form.tcl $(FORM)

$(FORMPROG): $(OBJDIR)/form.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/form.o $(DBMILIB) $(GISLIB) $(DATETIMELIB) \
				$(TCLTKLIBPATH) $(TCLTKLIBS) $(MATHLIB) $(XDRLIB)

