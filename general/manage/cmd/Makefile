
MODULE_TOPDIR = ../../..

HOME=$(BIN_CMD)

MLIBDEP = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(MANAGE_LIBNAME).$(LIB_SUFFIX)
MLIB = -l$(MANAGE_LIBNAME)
LIBES = $(VECTLIB) $(MLIB) $(DBMILIB) $(GISLIB)
LIST = $(OBJDIR)/init.o  
DEPENDENCIES = $(GISDEP) $(VECTDEP) $(DBMIDEP) $(MLIBDEP)

EXTRA_INC = -I..
include $(MODULE_TOPDIR)/include/Make/Module.make


default: \
	$(HOME)/g.copy \
	$(HOME)/g.list \
	$(HOME)/g.remove \
	$(HOME)/g.rename \
	htmlcmd

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

htmlcmd:
	for prog in $(PROGRAMS) ; do \
		GRASS_FAKE_START=1 GISBASE=$(GISBASE) LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(GISBASE)/lib $(ETC)/bin/cmd/$$prog --html-description | grep -v '</body>' > $$prog.tmp ; true ; \
		test ! -f $$prog.html || ( cat $$prog.html >> $$prog.tmp ) ; \
		echo "<HR>" >> $$prog.tmp ; \
		echo "<P><a href=index.html>Help Index</a>" >> $$prog.tmp ; \
		echo "</body></html>" >> $$prog.tmp ; \
		mkdir -p $(GISBASE)/docs/html ; \
		mv $$prog.tmp $(GISBASE)/docs/html/$$prog.html ; \
	done

