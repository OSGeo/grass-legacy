MODULE_TOPDIR = ../..

LIBES   = $(D_LIB) $(DISPLAYLIB) $(RASTERLIB) $(GISLIB)
DEPLIBS = $(D_DEP) $(DISPLAYDEP) $(RASTERDEP) $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

PGM=d.frame

default: \
	$(ETC)/frame.create \
	$(ETC)/frame.select \
	$(ETC)/frame.dumper \
	$(ETC)/frame.list \
	$(BIN)/$(PGM) \
	htmlcmd

$(ETC)/frame.create: $(OBJDIR)/create.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/create.o $(LIBES) $(MATHLIB) $(XDRLIB)

$(ETC)/frame.select: $(OBJDIR)/select.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/select.o $(LIBES) $(MATHLIB) $(XDRLIB)

$(ETC)/frame.dumper: $(OBJDIR)/dumper.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/dumper.o $(LIBES) $(MATHLIB) $(XDRLIB)

$(ETC)/frame.list: $(OBJDIR)/list.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/list.o $(LIBES) $(MATHLIB) $(XDRLIB)

$(BIN)/$(PGM): $(OBJDIR)/frame.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/frame.o $(LIBES) $(MATHLIB) $(XDRLIB)
