MODULE_TOPDIR = ../..

LIBES= $(GISLIB) $(IMAGERYLIB) $(GPROJLIB) $(GDAL_LIBS) $(DLLIB)
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

#rename main.c to main_in.c when main_out.c is written:
OBJ_IN = $(OBJDIR)/main.o\
	$(OBJDIR)/gbgetsymbol.o\
	$(OBJDIR)/gdalbridge.o\
	$(OBJDIR)/make_loc.o

INPGM  = r.in.gdal
#OUTPGM = r.out.gdal

default: $(BIN_CMD)/$(INPGM)
# $(BIN_CMD)/$(OUTPGM)

#   compile the command programs
$(BIN_CMD)/$(INPGM):  $(OBJ_IN) $(LIBES)
	\mkdir -p $(OBJDIR)
	$(CC) $(LDFLAGS) -o $@ $(OBJ_IN) $(LIBES) $(MATHLIB) $(XDRLIB)

#$(BIN_CMD)/$(OUTPGM):  $(OBJ_OUT) $(LIBES) 
#	$(CC) $(LDFLAGS) -o $@ $(OBJ_OUT) $(LIBES) $(MATHLIB) $(XDRLIB)
