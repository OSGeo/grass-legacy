
MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

DRIVER=$(DBDRIVERDIR)/pg

CRYPTLIB = -lcrypt

LIBES = $(DBMILIB) $(SQLPLIB) $(DBSTUBSLIB) $(GISLIB) $(DATETIMELIB) $(PQLIB) $(CRYPTLIB)
EXTRA_CFLAGS = $(PQINCPATH)

OBJS 	=  \
	db.o \
	driver.o \
	error.o \
	listtab.o \
	main.o \
	table.o \
	column.o \
	cursor.o \
	pgexe.o \
	describe.o \
	execute.o \
	fetch.o \
	select.o \
	str.o \
	
ARCH_OBJS := $(foreach obj,$(OBJS),$(OBJDIR)/$(obj))

default: $(DRIVER)

$(DRIVER): $(ARCH_OBJS) $(LIBES)
	$(CC) -g $(LDFLAGS) -o $@ $(ARCH_OBJS) $(LIBES) $(MATHLIB) $(XDRLIB)



