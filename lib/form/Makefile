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

# substitute OSX arch flags for tcltk
ifneq ($(MACOSX_ARCHS),)
TCFLAGS := $(subst $(MACOSX_ARCHS),,$(CFLAGS)) $(MACOSX_ARCHS_TCLTK)
TLDFLAGS := $(subst $(MACOSX_ARCHS),,$(LDFLAGS)) $(MACOSX_ARCHS_TCLTK)
else
TCFLAGS := $(CFLAGS)
TLDFLAGS := $(LDFLAGS)
endif

default: lib $(GTCLTKFORM)

$(FORMDIR):
	if [ ! -d $@ ]; then $(MKDIR) $@; fi

ifneq ($(BROKEN_MAKE),)
$(HTMLLIB): html_library_grass.tcl $(FORMDIR)
else
$(HTMLLIB): html_library_grass.tcl | $(FORMDIR)
endif
	$(INSTALL_DATA) $< $@

ifneq ($(BROKEN_MAKE),)
$(FORM): form.tcl $(FORMDIR)
else
$(FORM): form.tcl | $(FORMDIR)
endif
	$(INSTALL_DATA) $< $@

ifneq ($(BROKEN_MAKE),)
$(FORMPROG): $(OBJDIR)/form.o $(FORMDIR)
else
$(FORMPROG): $(OBJDIR)/form.o | $(FORMDIR)
endif
	$(CC) $(TLDFLAGS) -o $@ $(OBJDIR)/form.o $(DBMILIB) $(GISLIB) $(DATETIMELIB) \
				$(TCLTKLIBPATH) $(TCLTKLIBS) $(MATHLIB) $(XDRLIB)

ifeq ($(BROKEN_MAKE),)
$(OBJDIR)/form.o : form.c | $(OBJDIR)
	$(CC) $(TCFLAGS) $(EXTRA_CFLAGS) $(NLS_CFLAGS) $(INC) -o $@ -c $<
else
$(OBJDIR)/form.o : form.c
	$(MAKE) $(OBJDIR)
	$(CC) $(TCFLAGS) $(EXTRA_CFLAGS) $(NLS_CFLAGS) $(INC) -o $@ -c $<
endif
