
MODULE_TOPDIR = ../..

LIB_NAME = $(FORM_LIBNAME)
DEPENDENCIES = $(GRASS_INCDIR)/form.h

LIB_OBJS = generate.o open.o

HTMLLIB=$(GISBASE)/etc/form/html_library.tcl
FORM=$(GISBASE)/etc/form/form.tcl
FORMPROG=$(GISBASE)/etc/form/form
EXTRA_CFLAGS = $(TCLINCDIR) $(TKINCDIR)

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib $(HTMLLIB) $(FORM) $(FORMPROG) 

$(HTMLLIB): html_library_grass.tcl
	if [ ! -d $(GISBASE)/etc ]; then mkdir $(GISBASE)/etc; fi
	if [ ! -d $(GISBASE)/etc/form ]; then mkdir $(GISBASE)/etc/form; fi
	cp html_library_grass.tcl $(HTMLLIB)

$(FORM): form.tcl
	cp form.tcl $(FORM)

$(FORMPROG): $(OBJDIR)/form.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/form.o $(DBMILIB) $(GISLIB) $(DATETIMELIB) \
				$(TCLTKLIBPATH) $(TCLTKLIBS) $(MATHLIB) $(XDRLIB)

