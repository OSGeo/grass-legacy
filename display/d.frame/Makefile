MODULE_TOPDIR = ../..

LIBES   = $(D_LIB) $(DISPLAYLIB) $(RASTERLIB) $(GISLIB)
DEPLIBS = $(D_DEP) $(DISPLAYDEP) $(RASTERDEP) $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

HOME=$(BIN_CMD)

PGM=d.frame

default: \
	$(ETC)/frame.create \
	$(ETC)/frame.select \
	$(ETC)/frame.dumper \
	$(ETC)/frame.list \
	$(HOME)/$(PGM) \
	htmlcmd

$(ETC)/frame.create: $(OBJDIR)/create.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/create.o $(LIBES) $(MATHLIB) $(XDRLIB)

$(ETC)/frame.select: $(OBJDIR)/select.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/select.o $(LIBES) $(MATHLIB) $(XDRLIB)

$(ETC)/frame.dumper: $(OBJDIR)/dumper.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/dumper.o $(LIBES) $(MATHLIB) $(XDRLIB)

$(ETC)/frame.list: $(OBJDIR)/list.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/list.o $(LIBES) $(MATHLIB) $(XDRLIB)

$(HOME)/$(PGM): $(OBJDIR)/frame.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/frame.o $(LIBES) $(MATHLIB) $(XDRLIB)
	@test -x $(BIN)/d.frame || ln $(ETC)/front.end $(BIN)/d.frame

PROGRAMS = d.frame

htmlcmd:
	for prog in $(PROGRAMS) ; do \
		GRASS_FAKE_START=1 GISBASE=$(GISBASE) $(ETC)/bin/cmd/$$prog --html-description | grep -v '</body>' > $$prog.tmp ; true ; \
		test ! -f $$prog.html || ( cat $$prog.html >> $$prog.tmp ) ; \
		echo "<HR>" >> $$prog.tmp ; \
		echo "<P><a href=index.html>Help Index</a>" >> $$prog.tmp ; \
		echo "</body></html>" >> $$prog.tmp ; \
		mkdir -p $(GISBASE)/docs/html ; \
		mv $$prog.tmp $(GISBASE)/docs/html/$$prog.html ; \
	done
