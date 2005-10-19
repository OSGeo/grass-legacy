MODULE_TOPDIR = ../../..

LIBES = $(VECTLIB) $(DBMILIB) $(GISLIB) 
DEPENDENCIES = $(VECTDEP) $(DBMIDEP) $(GISDEP)
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)

include $(MODULE_TOPDIR)/include/Make/Module.make

PGM1=cell
PGM2=vector

default: $(MK) $(ETC)/lister/cell $(ETC)/lister/vector

MK:
	$(MKDIR) $(ETC)/lister

$(ETC)/lister/cell: $(OBJDIR)/cell.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/cell.o $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)

$(ETC)/lister/vector: $(OBJDIR)/vector.o $(LIST)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/vector.o $(LIST) $(LIBES) $(MATHLIB) $(XDRLIB)
