
MODULE_TOPDIR = ../..

HOME=$(BIN_CMD)

LIBES = $(DBMILIB) $(GISLIB) $(DATETIMELIB)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: \
	$(HOME)/db.columns \
	$(HOME)/db.copy \
	$(HOME)/db.createdb \
	$(HOME)/db.dropdb \
	$(HOME)/db.droptable \
	$(HOME)/db.databases \
	$(HOME)/db.describe \
	$(HOME)/db.drivers \
	$(HOME)/db.execute \
	$(HOME)/db.select \
	$(HOME)/db.tables \
	$(HOME)/db.connect \

$(HOME)/db.columns: $(OBJDIR)/columns.o $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/columns.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -e $(BIN)/db.columns || ln $(ETC)/front.end $(BIN)/db.columns

$(HOME)/db.copy: $(OBJDIR)/copy.o $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/copy.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -e $(BIN)/db.copy || ln $(ETC)/front.end $(BIN)/db.copy

$(HOME)/db.createdb: $(OBJDIR)/createdb.o $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/createdb.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -e $(BIN)/db.createdb || ln $(ETC)/front.end $(BIN)/db.createdb

$(HOME)/db.dropdb: $(OBJDIR)/dropdb.o $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/dropdb.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -e $(BIN)/db.dropdb || ln $(ETC)/front.end $(BIN)/db.dropdb

$(HOME)/db.droptable: $(OBJDIR)/droptable.o $(LIBES) 
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/droptable.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -e $(BIN)/db.droptable || ln $(ETC)/front.end $(BIN)/db.droptable

$(HOME)/db.databases: $(OBJDIR)/databases.o $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/databases.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -e $(BIN)/db.databases || ln $(ETC)/front.end $(BIN)/db.databases

$(HOME)/db.describe: $(OBJDIR)/describe.o $(OBJDIR)/printtab.o $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/describe.o $(OBJDIR)/printtab.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -e $(BIN)/db.describe || ln $(ETC)/front.end $(BIN)/db.describe

$(HOME)/db.drivers: $(OBJDIR)/drivers.o $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/drivers.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -e $(BIN)/db.drivers || ln $(ETC)/front.end $(BIN)/db.drivers

$(HOME)/db.execute: $(OBJDIR)/execute.o $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/execute.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -e $(BIN)/db.execute || ln $(ETC)/front.end $(BIN)/db.execute

$(HOME)/db.select: $(OBJDIR)/select.o $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/select.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -e $(BIN)/db.select || ln $(ETC)/front.end $(BIN)/db.select

$(HOME)/db.tables: $(OBJDIR)/tables.o $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/tables.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -e $(BIN)/db.tables || ln $(ETC)/front.end $(BIN)/db.tables

$(HOME)/db.connect: $(OBJDIR)/connect.o $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/connect.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -e $(BIN)/db.connect || ln $(ETC)/front.end $(BIN)/db.connect
	

