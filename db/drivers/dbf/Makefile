
MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

DRIVER=$(DBDRIVERDIR)/dbf

LIBES = $(DBMILIB) $(SQLPLIB) $(SHAPELIB) $(DBSTUBSLIB) $(GISLIB) $(DATETIMELIB)

OBJS 	=  \
	column.o \
	cursor.o \
	db.o \
	dbfexe.o \
	describe.o \
	driver.o \
	error.o \
	execute.o \
	fetch.o \
	listtab.o \
	main.o \
	select.o \
	str.o \
	table.o

D_OBJS 	=  \
	column.o \
	cursor.o \
	db.o \
	dbfexe.o \
	describe.o \
	driver.o \
	error.o \
	execute.o \
	fetch.o \
	listtab.o \
	main_debug.o \
	select.o \
	str.o \
	table.o
	
ARCH_OBJS := $(foreach obj,$(OBJS),$(OBJDIR)/$(obj))
D_ARCH_OBJS := $(foreach obj,$(D_OBJS),$(OBJDIR)/$(obj))

debug_driver: $(D_ARCH_OBJS) $(LIBES)
	$(CC) $(LDFLAGS) -o dbf $(D_ARCH_OBJS) $(LIBES) $(MATHLIB) $(XDRLIB)

default: $(DRIVER)

$(DRIVER): $(ARCH_OBJS) $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(ARCH_OBJS) $(LIBES) $(MATHLIB) $(XDRLIB)



