
MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

DRIVER=$(DBDRIVERDIR)/odbc
EXTRA_INC = $(ODBCINC)

LIBES = $(DBMILIB) $(DBSTUBSLIB) $(GISLIB) $(DATETIMELIB)

OBJS 	=  \
	connect.o \
	cursor.o \
	db.o \
	describe.o \
	driver.o \
	error.o \
	execute.o \
	fetch.o \
	listdb.o \
	listtab.o \
	main.o \
	select.o \
	table.o
	
ARCH_OBJS := $(foreach obj,$(OBJS),$(OBJDIR)/$(obj))

default: $(DRIVER)

$(DRIVER): $(ARCH_OBJS) $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(ARCH_OBJS) $(LIBES) $(ODBCLIB) $(MATHLIB) $(XDRLIB)

