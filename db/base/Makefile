
MODULE_TOPDIR = ../..

LIBES = $(DBMILIB) $(GISLIB) $(DATETIMELIB)
DEPENDENCIES = $(GISDEP) $(DBMIDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: \
	$(BIN)/db.columns \
	$(BIN)/db.copy \
	$(BIN)/db.describe \
	$(BIN)/db.drivers \
	$(BIN)/db.execute \
	$(BIN)/db.select \
	$(BIN)/db.tables \
	$(BIN)/db.connect \
	htmlmulti

$(BIN)/db.columns: $(OBJDIR)/columns.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/columns.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.copy: $(OBJDIR)/copy.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/copy.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.createdb: $(OBJDIR)/createdb.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/createdb.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.dropdb: $(OBJDIR)/dropdb.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/dropdb.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.droptable: $(OBJDIR)/droptable.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/droptable.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.databases: $(OBJDIR)/databases.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/databases.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.describe: $(OBJDIR)/describe.o $(OBJDIR)/printtab.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/describe.o $(OBJDIR)/printtab.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.drivers: $(OBJDIR)/drivers.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/drivers.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.execute: $(OBJDIR)/execute.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/execute.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.select: $(OBJDIR)/select.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/select.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.tables: $(OBJDIR)/tables.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/tables.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.connect: $(OBJDIR)/connect.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/connect.o $(LIBES) $(XDRLIB) $(MATHLIB)

#not used: db.createdb db.dropdb db.databases
PROGRAMS = db.columns db.copy db.droptable db.describe db.drivers db.execute db.select db.tables db.connect

