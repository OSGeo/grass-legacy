MODULE_TOPDIR = ../../..

DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

DRIVER=$(DBDRIVERDIR)/pg

LIBES = $(DBMIDRIVERLIB) $(DBMIBASELIB) $(DBMIEXTRALIB) $(SQLPLIB) $(DBSTUBSLIB) \
	$(GISLIB) $(DATETIMELIB) $(PQLIB)

EXTRA_CFLAGS = $(PQINCPATH) $(TCLINCDIR) $(TKINCDIR)
EXTRA_INC = -I$(GRASS_LIBDIR)/db/dbmi_driver

OBJS = $(subst .c,.o,$(wildcard *.c))
	
ARCH_OBJS := $(foreach obj,$(OBJS),$(OBJDIR)/$(obj))

default: $(DRIVER)

$(DRIVER): $(ARCH_OBJS)
	$(CC) $(LDFLAGS) -o $@ $(ARCH_OBJS) $(PQLIBPATH) $(LIBES) $(TCLTKLIBPATH) $(TCLTKLIBS) \
		$(MATHLIB) $(XDRLIB)
