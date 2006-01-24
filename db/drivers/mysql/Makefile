
MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

DRIVER=$(DBDRIVERDIR)/mysql$(EXE_SUFFIX)

LIBES = $(DBMIDRIVERLIB) $(DBMIBASELIB) $(SQLPLIB) $(DBSTUBSLIB) \
	 $(GISLIB) $(DATETIMELIB) 

EXTRA_CFLAGS = $(MYSQLINCPATH) $(TCLINCDIR) $(TKINCDIR)
EXTRA_INC = -I$(GRASS_LIBDIR)/db/dbmi_driver

DEPENDENCIES = $(GISDEP)

OBJS 	=  \
	create_table.o \
	cursor.o \
	db.o \
	describe.o \
	driver.o \
	error.o \
	execute.o \
	fetch.o \
	index.o \
	listtab.o \
	main.o \
	parse.o	\
	select.o 

ARCH_OBJS := $(foreach obj,$(OBJS),$(OBJDIR)/$(obj))

default: $(DRIVER)

$(DRIVER): $(ARCH_OBJS)
	$(CC) -g $(LDFLAGS) -o $@ $(ARCH_OBJS) $(LIBES) $(MYSQLLIBPATH) $(MYSQLLIB) \
		$(TCLTKLIBPATH) $(TCLTKLIBS) $(MATHLIB) $(XDRLIB)
