
MODULE_TOPDIR = ../../..

DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

DRIVER=$(DBDRIVERDIR)/pg

CRYPTLIB = -lcrypt

LIBES = $(DBMIDRIVERLIB) $(DBMIBASELIB) $(SQLPLIB) $(DBSTUBSLIB) \
	-l$(DBDIALOG_LIBNAME) \
	$(GISLIB) $(DATETIMELIB) $(PQLIB)
EXTRA_CFLAGS = $(PQINCPATH) $(TCLINCDIR) $(TKINCDIR)

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
	pgexe.o \
	select.o \
	str.o \
	table.o
	
ARCH_OBJS := $(foreach obj,$(OBJS),$(OBJDIR)/$(obj))

default: $(DRIVER)

$(DRIVER): $(ARCH_OBJS)
	$(CC) -g $(LDFLAGS) -o $@ $(ARCH_OBJS) $(PQLIBPATH) $(LIBES) $(CRYPTLIB)  $(TCLTKLIBPATH) $(TCLTKLIBS) \
		$(MATHLIB) $(XDRLIB)



