MODULE_TOPDIR = ../..

LIBES   = $(D_LIB) $(DISPLAYLIB) $(RASTERLIB) $(GISLIB)
DEPLIBS = $(D_DEP) $(DISPLAYDEP) $(RASTERDEP) $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: \
	$(ETC)/frame.create \
	$(ETC)/frame.select \
	$(ETC)/frame.dumper \
	$(ETC)/frame.list \
	$(BIN_CMD)/d.frame

$(ETC)/frame.create: create.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/create.o $(LIBES) $(MATHLIB) $(XDRLIB)

$(ETC)/frame.select: select.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/select.o $(LIBES) $(MATHLIB) $(XDRLIB)

$(ETC)/frame.dumper: dumper.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/dumper.o $(LIBES) $(MATHLIB) $(XDRLIB)

$(ETC)/frame.list: list.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/list.o $(LIBES) $(MATHLIB) $(XDRLIB)

$(BIN_CMD)/d.frame: frame.o $(DEPLIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/frame.o $(LIBES) $(MATHLIB) $(XDRLIB)
	@test -x $(BIN)/d.frame || ln $(ETC)/front.end $(BIN)/d.frame
