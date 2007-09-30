MODULE_TOPDIR = ../..

EXTRA_LIBS=$(GISLIB) $(DBMILIB)

LIB_NAME = $(FORM_LIBNAME)

FORMDIR=$(GISBASE)/etc/form

HTMLLIB=$(FORMDIR)/html_library.tcl
FORM=$(FORMDIR)/form.tcl
FORMPROG=$(FORMDIR)/form
EXTRA_CFLAGS = $(XCFLAGS) $(TCLINCDIR) $(TKINCDIR)

include $(MODULE_TOPDIR)/include/Make/Platform.make

LIB_OBJS = generate.o open.o
ifneq ($(TCLTKLIBS),)
    GTCLTKFORM = $(HTMLLIB) $(FORM) $(FORMPROG) 
endif

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib $(GTCLTKFORM)

$(FORMDIR):
	if [ ! -d $@ ]; then $(MKDIR) $@; fi

$(HTMLLIB): html_library_grass.tcl | $(FORMDIR)
	$(INSTALL_DATA) $< $@

$(FORM): form.tcl | $(FORMDIR)
	$(INSTALL_DATA) $< $@

$(FORMPROG): $(OBJDIR)/form.o | $(FORMDIR)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/form.o $(DBMILIB) $(GISLIB) $(DATETIMELIB) \
				$(TCLTKLIBPATH) $(TCLTKLIBS) $(MATHLIB) $(XDRLIB)
