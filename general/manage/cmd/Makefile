
MODULE_TOPDIR = ../../..

HOME=$(BIN_CMD)

MLIB = $(ARCH_LIBDIR)/$(LIB_PREFIX)manage.$(LIB_SUFFIX)
LIBES = $(VECTLIB) $(MLIB) $(DBMILIB) $(GISLIB)
LIST = $(OBJDIR)/init.o $(MLIB)
DEPENDENCIES = $(GISDEP) $(VECTDEP) $(DBMIDEP)

EXTRA_INC = -I..
include $(MODULE_TOPDIR)/include/Make/Module.make


default: \
	$(HOME)/g.copy \
	$(HOME)/g.list \
	$(HOME)/g.remove \
	$(HOME)/g.rename \
	allhtml

$(HOME)/g.copy: $(OBJDIR)/copy.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/copy.o $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)
	@test -x $(BIN)/g.copy || ln $(ETC)/front.end $(BIN)/g.copy

$(HOME)/g.list: $(OBJDIR)/list.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/list.o $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)
	@test -x $(BIN)/g.list || ln $(ETC)/front.end $(BIN)/g.list

$(HOME)/g.remove: $(OBJDIR)/remove.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/remove.o $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)
	@test -x $(BIN)/g.remove || ln $(ETC)/front.end $(BIN)/g.remove

$(HOME)/g.rename: $(OBJDIR)/rename.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/rename.o $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)
	@test -x $(BIN)/g.rename || ln $(ETC)/front.end $(BIN)/g.rename

PROGRAMS = g.copy g.list g.remove g.rename

allhtml: *.html
	for pgm in $(PROGRAMS) ; do \
		GRASS_FAKE_START=1 GISBASE=$(GISBASE) $(ETC)/bin/cmd/$$pgm --html-description | grep -v '</body>' > $$pgm.tmp ; true \
		test ! -f $$pgm.html || ( cat $$pgm.html >> $$pgm.tmp ) ; \
		echo "</body></html>" >> $$pgm.tmp ; \
		mkdir -p $(GISBASE)/docs/html ; \
		mv $$pgm.tmp $(GISBASE)/docs/html/$$pgm.html ; \
	done
