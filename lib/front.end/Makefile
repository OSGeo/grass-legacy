
MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

default: $(ETC)/front.end

$(ETC)/front.end: $(OBJDIR)/main.o
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/main.o $(GISLIB) $(DATETIMELIB) $(MATHLIB) $(XDRLIB)

