
MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

DRIVER=$(DBDRIVERDIR)/mysql

LIBES = $(DBMIDRIVERLIB) $(DBMIBASELIB) $(SQLPLIB) $(DBSTUBSLIB) \
	 $(GISLIB) $(DATETIMELIB) 

EXTRA_CFLAGS = $(MYSQLINCPATH) $(TCLINCDIR) $(TKINCDIR)
EXTRA_INC = -I$(GRASS_LIBDIR)/db/dbmi_driver

DEPENDENCIES = $(GISDEP)

OBJS 	=  \
	column.o \
	cursor.o \
	db.o \
	describe.o \
	driver.o \
	execute.o \
	error.o \
	fetch.o \
	listdb.o \
	listtab.o \
	main.o \
	mysqlexe.o \
	select.o \
	str.o \
	table.o \
	create_table.o

ARCH_OBJS := $(foreach obj,$(OBJS),$(OBJDIR)/$(obj))

default: $(DRIVER)

$(DRIVER): $(ARCH_OBJS)
	$(CC) -g $(LDFLAGS) -o $@ $(ARCH_OBJS) $(LIBES) $(MYSQLLIBPATH) $(MYSQLLIB) \
		$(TCLTKLIBPATH) $(TCLTKLIBS) $(MATHLIB) $(XDRLIB)
