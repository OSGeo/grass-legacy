MODULE_TOPDIR = ../../..

LIB_NAME = $(GRAPH_LIBNAME)

LIB_OBJS = \
	avl.o \
	tavl.o \
	graph.o \
	heap.o \
	tree.o \
	helpers.o \
	graph_v1.o \
	graph_v2.o

include $(MODULE_TOPDIR)/include/Make/Lib.make

DGLINC = $(ARCH_INCDIR)/dgl/

default: $(DGLINC)/avl.h $(DGLINC)/tavl.h $(DGLINC)/graph.h $(DGLINC)/heap.h \
	 $(DGLINC)/tree.h $(DGLINC)/type.h $(DGLINC)/helpers.h $(DGLINC)/graph_v1.h $(DGLINC)/graph_v2.h \
	 $(ARCH_INCDIR)/dgl.h lib

$(DGLINC)/avl.h: avl.h
	$(MKDIR) $(DGLINC)
	$(INSTALL) -m 644 avl.h $(DGLINC)/avl.h

$(DGLINC)/tavl.h: tavl.h
	$(INSTALL) -m 644 tavl.h $(DGLINC)/tavl.h

$(DGLINC)/heap.h: heap.h
	$(INSTALL) -m 644 heap.h $(DGLINC)/heap.h

$(DGLINC)/graph.h: graph.h
	$(INSTALL) -m 644 graph.h $(DGLINC)/graph.h

$(DGLINC)/tree.h: tree.h
	$(INSTALL) -m 644 tree.h $(DGLINC)/tree.h

$(DGLINC)/type.h: type.h
	$(INSTALL) -m 644 type.h $(DGLINC)/type.h

$(DGLINC)/graph_v1.h: graph_v1.h
	$(INSTALL) -m 644 graph_v1.h $(DGLINC)/graph_v1.h

$(DGLINC)/graph_v2.h: graph_v2.h
	$(INSTALL) -m 644 graph_v2.h $(DGLINC)/graph_v2.h

$(DGLINC)/helpers.h: helpers.h
	$(INSTALL) -m 644 helpers.h $(DGLINC)/helpers.h

$(ARCH_INCDIR)/dgl.h: dgl.h
	$(INSTALL) -m 644 dgl.h $(ARCH_INCDIR)/dgl.h

