
MODULE_TOPDIR = ../../..

HOME=$(BIN_CMD)

LIBES = $(MANAGELIB) $(VECTLIB) $(DBMILIB) $(GISLIB)
LIST = $(OBJDIR)/init.o  
DEPENDENCIES = $(GISDEP) $(VECTDEP) $(DBMIDEP) $(MANAGEDEP)

EXTRA_INC = -I..
include $(MODULE_TOPDIR)/include/Make/Module.make


default: \
	$(HOME)/g.copy \
	$(HOME)/g.list \
	$(HOME)/g.remove \
	$(HOME)/g.rename \
	htmlmulti

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

