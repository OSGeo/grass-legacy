
MODULE_TOPDIR = ../..

LIBES = $(DBMILIB) $(GISLIB) $(DATETIMELIB)
DEPENDENCIES = $(GISDEP) $(DBMIDEP)
EXTRA_LIBS = $(XDRLIB)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: \
	$(BIN)/db.columns$(EXE_SUFFIX) \
	$(BIN)/db.copy$(EXE_SUFFIX) \
	$(BIN)/db.describe$(EXE_SUFFIX) \
	$(BIN)/db.drivers$(EXE_SUFFIX) \
	$(BIN)/db.execute$(EXE_SUFFIX) \
	$(BIN)/db.select$(EXE_SUFFIX) \
	$(BIN)/db.tables$(EXE_SUFFIX) \
	$(BIN)/db.connect$(EXE_SUFFIX) \
	htmlmulti

$(BIN)/db.columns$(EXE_SUFFIX): $(OBJDIR)/columns.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/columns.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.copy$(EXE_SUFFIX): $(OBJDIR)/copy.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/copy.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.createdb$(EXE_SUFFIX): $(OBJDIR)/createdb.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/createdb.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.dropdb$(EXE_SUFFIX): $(OBJDIR)/dropdb.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/dropdb.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.droptable$(EXE_SUFFIX): $(OBJDIR)/droptable.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/droptable.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.databases$(EXE_SUFFIX): $(OBJDIR)/databases.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/databases.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.describe$(EXE_SUFFIX): $(OBJDIR)/describe.o $(OBJDIR)/printtab.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/describe.o $(FMODE_OBJ) $(OBJDIR)/printtab.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.drivers$(EXE_SUFFIX): $(OBJDIR)/drivers.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/drivers.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.execute$(EXE_SUFFIX): $(OBJDIR)/execute.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/execute.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.select$(EXE_SUFFIX): $(OBJDIR)/select.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/select.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.tables$(EXE_SUFFIX): $(OBJDIR)/tables.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/tables.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/db.connect$(EXE_SUFFIX): $(OBJDIR)/connect.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/connect.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

#not used: db.createdb db.dropdb db.databases
PROGRAMS = db.columns db.copy db.droptable db.describe db.drivers db.execute db.select db.tables db.connect

