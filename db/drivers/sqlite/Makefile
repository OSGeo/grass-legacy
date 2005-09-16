MODULE_TOPDIR = ../../..

DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

DRIVER=$(DBDRIVERDIR)/sqlite

LIBES = $(DBMIDRIVERLIB) $(DBMIBASELIB) $(DBMIEXTRALIB) $(DBSTUBSLIB) \
	$(GISLIB) $(DATETIMELIB) $(SQLITELIB)

EXTRA_CFLAGS = $(SQLITEINCPATH) 
EXTRA_INC = -I$(GRASS_LIBDIR)/db/dbmi_driver

OBJS = $(subst .c,.o,$(wildcard *.c))
	
ARCH_OBJS := $(foreach obj,$(OBJS),$(OBJDIR)/$(obj))

default: $(DRIVER)

$(DRIVER): $(ARCH_OBJS)
	$(CC) $(LDFLAGS) -o $@ $(ARCH_OBJS) $(LIBES) \
		$(MATHLIB) $(XDRLIB)
