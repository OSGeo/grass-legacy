
MODULE_TOPDIR = ../..

HOME=$(BIN_CMD)

LIBES = $(DBMILIB) $(GISLIB) $(DATETIMELIB)
DEPENDENCIES = $(GISDEP) $(DBMIDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: \
	$(HOME)/db.columns \
	$(HOME)/db.copy \
	$(HOME)/db.droptable \
	$(HOME)/db.describe \
	$(HOME)/db.drivers \
	$(HOME)/db.execute \
	$(HOME)/db.select \
	$(HOME)/db.tables \
	$(HOME)/db.connect \
	htmlcmd

$(HOME)/db.columns: $(OBJDIR)/columns.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/columns.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -x $(BIN)/db.columns || ln $(ETC)/front.end $(BIN)/db.columns

$(HOME)/db.copy: $(OBJDIR)/copy.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/copy.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -x $(BIN)/db.copy || ln $(ETC)/front.end $(BIN)/db.copy

$(HOME)/db.createdb: $(OBJDIR)/createdb.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/createdb.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -x $(BIN)/db.createdb || ln $(ETC)/front.end $(BIN)/db.createdb

$(HOME)/db.dropdb: $(OBJDIR)/dropdb.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/dropdb.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -x $(BIN)/db.dropdb || ln $(ETC)/front.end $(BIN)/db.dropdb

$(HOME)/db.droptable: $(OBJDIR)/droptable.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/droptable.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -x $(BIN)/db.droptable || ln $(ETC)/front.end $(BIN)/db.droptable

$(HOME)/db.databases: $(OBJDIR)/databases.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/databases.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -x $(BIN)/db.databases || ln $(ETC)/front.end $(BIN)/db.databases

$(HOME)/db.describe: $(OBJDIR)/describe.o $(OBJDIR)/printtab.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/describe.o $(OBJDIR)/printtab.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -x $(BIN)/db.describe || ln $(ETC)/front.end $(BIN)/db.describe

$(HOME)/db.drivers: $(OBJDIR)/drivers.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/drivers.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -x $(BIN)/db.drivers || ln $(ETC)/front.end $(BIN)/db.drivers

$(HOME)/db.execute: $(OBJDIR)/execute.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/execute.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -x $(BIN)/db.execute || ln $(ETC)/front.end $(BIN)/db.execute

$(HOME)/db.select: $(OBJDIR)/select.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/select.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -x $(BIN)/db.select || ln $(ETC)/front.end $(BIN)/db.select

$(HOME)/db.tables: $(OBJDIR)/tables.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/tables.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -x $(BIN)/db.tables || ln $(ETC)/front.end $(BIN)/db.tables

$(HOME)/db.connect: $(OBJDIR)/connect.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/connect.o $(LIBES) $(XDRLIB) $(MATHLIB)
	@test -x $(BIN)/db.connect || ln $(ETC)/front.end $(BIN)/db.connect

#not used: db.createdb db.dropdb db.databases
PROGRAMS = db.columns db.copy db.droptable db.describe db.drivers db.execute db.select db.tables db.connect

htmlcmd:
	for prog in $(PROGRAMS) ; do \
		GRASS_FAKE_START=1 GISBASE=$(GISBASE) LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(GISBASE)/lib $(ETC)/bin/cmd/$$prog --html-description | grep -v '</body>' > $$prog.tmp ; true ; \
		test ! -f $$prog.html || ( cat $$prog.html >> $$prog.tmp ) ; \
		echo "<HR>" >> $$prog.tmp ; \
		echo "<P><a href=index.html>Help Index</a>" >> $$prog.tmp ; \
		echo "</body></html>" >> $$prog.tmp ; \
		$(MKDIR) $(GISBASE)/docs/html ; \
		$(INSTALL) -m 644 $$prog.tmp $(GISBASE)/docs/html/$$prog.html ; \
	done
