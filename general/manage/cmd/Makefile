
MODULE_TOPDIR = ../../..

LIBES = $(MANAGELIB) $(VECTLIB) $(DBMILIB) $(GISLIB)
LIST = $(OBJDIR)/init.o  
DEPENDENCIES = $(GISDEP) $(VECTDEP) $(DBMIDEP) $(MANAGEDEP)

EXTRA_INC = -I..
include $(MODULE_TOPDIR)/include/Make/Module.make


default: \
	$(BIN)/g.copy \
	$(BIN)/g.list \
	$(BIN)/g.remove \
	$(BIN)/g.rename \
	htmlmulti

$(BIN)/g.copy: $(OBJDIR)/copy.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/copy.o $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)

$(BIN)/g.list: $(OBJDIR)/list.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/list.o $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)

$(BIN)/g.remove: $(OBJDIR)/remove.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/remove.o $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)

$(BIN)/g.rename: $(OBJDIR)/rename.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/rename.o $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)

PROGRAMS = g.copy g.list g.remove g.rename

