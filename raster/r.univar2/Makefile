
MODULE_TOPDIR = ../..

LIBES = $(G3DLIB) $(GISLIB)
DEPENDENCIES = $(G3DDEP) $(GISDEP)


include $(MODULE_TOPDIR)/include/Make/Module.make

R3UNIVAR = $(BIN)/r3.univar$(EXE)
RUNIVAR = $(BIN)/r.univar$(EXE)

default: \
	$(R3UNIVAR) \
	$(RUNIVAR) \
	htmlmulti

$(RUNIVAR): $(OBJDIR)/r.univar_main.o $(OBJDIR)/sort.o $(OBJDIR)/stats.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/r.univar_main.o $(OBJDIR)/sort.o $(OBJDIR)/stats.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(R3UNIVAR): $(OBJDIR)/r3.univar_main.o $(OBJDIR)/sort.o $(OBJDIR)/stats.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/r3.univar_main.o $(OBJDIR)/sort.o $(OBJDIR)/stats.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

#needed for htmlmulti
PROGRAMS = r.univar r3.univar

