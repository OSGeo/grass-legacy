
MODULE_TOPDIR = ../../..

LIB_NAME = $(GRAPH_LIBNAME)

LIB_OBJS = \
	bst.o \
	graph.o \
	heap.o \
	tree.o 

include $(MODULE_TOPDIR)/include/Make/Lib.make

DGLINC = $(ARCH_INCDIR)/dgl/

default: $(DGLINC)/bst.h $(DGLINC)/graph.h $(DGLINC)/heap.h \
	 $(DGLINC)/tree.h $(DGLINC)/type.h $(ARCH_INCDIR)/dgl.h lib

$(DGLINC)/bst.h: bst.h
	mkdir $(DGLINC)
	cp bst.h $(DGLINC)/bst.h

$(DGLINC)/heap.h: heap.h
	cp heap.h $(DGLINC)/heap.h

$(DGLINC)/graph.h: graph.h
	cp graph.h $(DGLINC)/graph.h

$(DGLINC)/tree.h: tree.h
	cp tree.h $(DGLINC)/tree.h

$(DGLINC)/type.h: type.h
	cp type.h $(DGLINC)/type.h

$(ARCH_INCDIR)/dgl.h: dgl.h
	cp dgl.h $(ARCH_INCDIR)/dgl.h

