
MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

DRIVER=$(DBDRIVERDIR)/mysql

LIBES = $(DBMILIB) $(SQLPLIB) $(DBSTUBSLIB) \
	$(ARCH_LIBDIR)/$(LIB_PREFIX)dbdialog.$(LIB_SUFFIX) \
	 $(GISLIB) $(DATETIMELIB) 
EXTRA_CFLAGS = $(MYSQLINCPATH) $(TCLINCDIR) $(TKINCDIR)

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
	table.o

ARCH_OBJS := $(foreach obj,$(OBJS),$(OBJDIR)/$(obj))

default: $(DRIVER)

$(DRIVER): $(ARCH_OBJS) $(LIBES)
	$(CC) -g $(LDFLAGS) -o $@ $(ARCH_OBJS) $(LIBES) $(MYSQLLIBPATH) $(MYSQLLIB) \
		$(TCLTKLIBPATH) $(TCLTKLIBS) $(MATHLIB) $(XDRLIB)
