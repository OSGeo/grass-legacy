
MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

default:$(ETC)/mon.list \
	$(ETC)/mon.release \
	$(ETC)/mon.select \
	$(ETC)/mon.start \
	$(ETC)/mon.status \
	$(ETC)/mon.stop \
	$(ETC)/mon.which

$(ETC)/mon.list:	$(OBJDIR)/list.o 
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/list.o $(RASTERLIB) $(GISLIB) $(MATHLIB) $(XDRLIB)
$(ETC)/mon.start:	$(OBJDIR)/start.o $(OBJDIR)/ttyname.o 
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/start.o $(OBJDIR)/ttyname.o $(RASTERLIB) $(GISLIB) $(MATHLIB) $(XDRLIB)
$(ETC)/mon.status:	$(OBJDIR)/status.o 
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/status.o $(RASTERLIB) $(GISLIB) $(MATHLIB) $(XDRLIB)
$(ETC)/mon.stop:	$(OBJDIR)/stop.o 
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/stop.o $(RASTERLIB) $(GISLIB) $(MATHLIB) $(XDRLIB)
$(ETC)/mon.which:	$(OBJDIR)/which.o 
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/which.o $(GISLIB) $(MATHLIB) $(XDRLIB) 
$(ETC)/mon.select:	$(OBJDIR)/select.o 
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/select.o $(D_LIB) $(DISPLAYLIB) $(RASTERLIB) $(GISLIB) $(MATHLIB) $(XDRLIB)  
$(ETC)/mon.release:	$(OBJDIR)/release.o 
	$(CC) $(LDFLAGS) -o $@ $(OBJDIR)/release.o $(RASTERLIB) $(GISLIB)  $(MATHLIB) $(XDRLIB)

#list.o: $(SRC)/include/monitors.h
#release.o: $(SRC)/include/monitors.h
#release.o: open.h
#select.o: $(SRC)/include/monitors.h
#start.o: $(SRC)/include/monitors.h
#status.o: $(SRC)/include/monitors.h
#status.o: open.h
#stop.o: $(SRC)/include/monitors.h
#stop.o: open.h

