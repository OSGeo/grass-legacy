
MODULE_TOPDIR = ../..

DEPENDENCIES = $(VECTDEP) $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

A_B_OFILES = \
	$(OBJDIR)/in.o\
	$(OBJDIR)/a2b.o

B_A_OFILES = \
	$(OBJDIR)/out.o\
	$(OBJDIR)/b2a.o


INPGM  = v.in.ascii 
OUTPGM = v.out.ascii 

default: $(BIN_CMD)/$(INPGM) $(BIN_CMD)/$(OUTPGM)

#   compile the command programs

$(BIN_CMD)/$(INPGM):  $(A_B_OFILES) $(VECTLIB) $(GISLIB)
	$(CC) $(LDFLAGS) -o $@ $(A_B_OFILES) $(VECTLIB) $(GISLIB) $(MATHLIB) $(XDRLIB)
	@test -e $(BIN)/$(INPGM) || ln $(ETC)/front.end $(BIN)/$(INPGM)

$(BIN_CMD)/$(OUTPGM):  $(B_A_OFILES) $(VECTLIB) $(GISLIB) 
	$(CC) $(LDFLAGS) -o $@ $(B_A_OFILES) $(VECTLIB) $(GISLIB)  $(MATHLIB) $(XDRLIB)
	@test -e $(BIN)/$(OUTPGM) || ln $(ETC)/front.end $(BIN)/$(OUTPGM)



