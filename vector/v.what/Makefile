
MODULE_TOPDIR = ../..

PGM = v.what

LIBES        = $(VECTLIB) $(FORMLIB) $(DBMILIB) $(GISLIB)

DEPENDENCIES = $(VECTDEP) $(DBMIDEP) $(FORMDEP) $(GISDEP)

EXTRA_CFLAGS = $(VECT_CFLAGS)
EXTRA_INC = $(VECT_INC)

include $(MODULE_TOPDIR)/include/Make/Module.make

ifeq ($(OPENGL_AQUA),1)
ifneq ($(TCLTKAQUA_IS64BIT),1)
CFLAGS := $(subst -arch ppc64,,$(subst -arch x86_64,,$(CFLAGS)))
LDFLAGS := $(subst -arch ppc64,,$(subst -arch x86_64,,$(LDFLAGS)))
endif
endif

default: cmd




