
MODULE_TOPDIR = ../../..

HOME=$(BIN_CMD)

MLIB = $(ARCH_LIBDIR)/$(LIB_PREFIX)manage.$(LIB_SUFFIX)
LIBES = $(VECTLIB) $(MLIB) $(DBMILIB) $(GISLIB)
LIST = $(OBJDIR)/init.o $(MLIB)
DEPENDENCIES = $(GISDEP) $(VECTDEP) $(DBMIDEP)

EXTRA_INCL = -I..
include $(MODULE_TOPDIR)/include/Make/Module.make


default: \
	$(HOME)/g.copy \
	$(HOME)/g.list \
	$(HOME)/g.remove \
	$(HOME)/g.rename

$(HOME)/g.copy: $(OBJDIR)/copy.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/copy.o $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)
	@test -e $(BIN)/g.copy || ln $(ETC)/front.end $(BIN)/g.copy

$(HOME)/g.list: $(OBJDIR)/list.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/list.o $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)
	@test -e $(BIN)/g.list || ln $(ETC)/front.end $(BIN)/g.list

$(HOME)/g.remove: $(OBJDIR)/remove.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/remove.o $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)
	@test -e $(BIN)/g.remove || ln $(ETC)/front.end $(BIN)/g.remove

$(HOME)/g.rename: $(OBJDIR)/rename.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/rename.o $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)
	@test -e $(BIN)/g.rename || ln $(ETC)/front.end $(BIN)/g.rename
