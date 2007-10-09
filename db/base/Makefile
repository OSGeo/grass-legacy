
MODULE_TOPDIR = ../..

LIBES = $(DBMILIB) $(GISLIB) $(DATETIMELIB)
DEPENDENCIES = $(GISDEP) $(DBMIDEP)
EXTRA_LIBS = $(XDRLIB)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: \
	$(BIN)/db.columns$(EXE) \
	$(BIN)/db.copy$(EXE) \
	$(BIN)/db.describe$(EXE) \
	$(BIN)/db.drivers$(EXE) \
	$(BIN)/db.execute$(EXE) \
	$(BIN)/db.select$(EXE) \
	$(BIN)/db.tables$(EXE) \
	$(BIN)/db.connect$(EXE) \
	htmlmulti

$(BIN)/db.columns$(EXE): $(OBJDIR)/columns.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/columns.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.copy$(EXE): $(OBJDIR)/copy.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/copy.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.createdb$(EXE): $(OBJDIR)/createdb.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/createdb.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.dropdb$(EXE): $(OBJDIR)/dropdb.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/dropdb.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.droptable$(EXE): $(OBJDIR)/droptable.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/droptable.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.databases$(EXE): $(OBJDIR)/databases.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/databases.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.describe$(EXE): $(OBJDIR)/describe.o $(OBJDIR)/printtab.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/describe.o $(FMODE_OBJ) $(OBJDIR)/printtab.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.drivers$(EXE): $(OBJDIR)/drivers.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/drivers.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.execute$(EXE): $(OBJDIR)/execute.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/execute.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.select$(EXE): $(OBJDIR)/select.o $(OBJDIR)/printtab.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/select.o $(FMODE_OBJ) $(OBJDIR)/printtab.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.tables$(EXE): $(OBJDIR)/tables.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/tables.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.connect$(EXE): $(OBJDIR)/connect.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/connect.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

#not used: db.createdb db.dropdb db.databases db.droptable
PROGRAMS = db.columns db.copy db.describe db.drivers db.execute db.select db.tables db.connect

