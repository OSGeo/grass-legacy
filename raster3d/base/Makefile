
MODULE_TOPDIR = ../..

LIBES = $(G3DLIB) $(GISLIB)
DEPENDENCIES = $(G3DDEP) $(GISDEP)


include $(MODULE_TOPDIR)/include/Make/Module.make

default: \
	$(BIN)/r3.info \
	$(BIN)/r3.mask \
	$(BIN)/r3.null \
	$(BIN)/r3.timestamp \
	htmlmulti

$(BIN)/r3.mask: $(OBJDIR)/r3.mask.main.o $(OBJDIR)/mask_functions.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/r3.mask.main.o $(OBJDIR)/mask_functions.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/r3.null: $(OBJDIR)/r3.null.main.o $(OBJDIR)/mask_functions.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/r3.null.main.o $(FMODE_OBJ) $(OBJDIR)/mask_functions.o $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/r3.timestamp: $(OBJDIR)/r3.timestamp.main.o 
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/r3.timestamp.main.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

$(BIN)/r3.info: $(OBJDIR)/r3.info.main.o 
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/r3.info.main.o $(FMODE_OBJ) $(LIBES) $(XDRLIB) $(MATHLIB)

#needed for htmlmulti
PROGRAMS = r3.info r3.mask r3.null r3.timestamp

