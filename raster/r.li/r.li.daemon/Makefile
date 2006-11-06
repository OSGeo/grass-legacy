MODULE_TOPDIR = ../../..

PGM = r.li.daemon

LIBES = $(GISLIB) 
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd
	rm -f libr_li.a
	ar r libr_li.a $(OBJDIR)/daemon.o
	ar r libr_li.a $(OBJDIR)/list.o
	ar r libr_li.a $(OBJDIR)/ipc.o
	ar r libr_li.a $(OBJDIR)/worker.o
	ar r libr_li.a $(OBJDIR)/GenericCell.o	
	ar r libr_li.a $(OBJDIR)/avl.o
	ar r libr_li.a $(OBJDIR)/avlID.o
