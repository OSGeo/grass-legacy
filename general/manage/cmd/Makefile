
MODULE_TOPDIR = ../../..

LIBES = $(MANAGELIB) $(VECTLIB) $(DBMILIB) $(GISLIB) $(G3DLIB)
LIST = $(OBJDIR)/init.o  
DEPENDENCIES = $(GISDEP) $(VECTDEP) $(DBMIDEP) $(MANAGEDEP) $(G3DDEP)

EXTRA_INC = -I..
include $(MODULE_TOPDIR)/include/Make/Module.make

COPY_MOD = $(BIN)/g.copy$(EXE)
LIST_MOD = $(BIN)/g.list$(EXE)
REMOVE_MOD = $(BIN)/g.remove$(EXE)
RENAME_MOD = $(BIN)/g.rename$(EXE)

default: htmlmulti

htmlmulti: $(COPY_MOD) $(LIST_MOD) $(REMOVE_MOD) $(RENAME_MOD)

$(COPY_MOD): $(OBJDIR)/copy.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/copy.o $(FMODE_OBJ) $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)

$(LIST_MOD): $(OBJDIR)/list.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/list.o $(FMODE_OBJ) $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)

$(REMOVE_MOD): $(OBJDIR)/remove.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/remove.o $(FMODE_OBJ) $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)

$(RENAME_MOD): $(OBJDIR)/rename.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/rename.o $(FMODE_OBJ) $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)

PROGRAMS = g.copy g.list g.remove g.rename

