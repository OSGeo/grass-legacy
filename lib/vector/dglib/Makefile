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
DGLINC_OLD = $(ARCH_DISTDIR)/include/dgl/

default: $(DGLINC)/avl.h $(DGLINC)/tavl.h $(DGLINC)/graph.h $(DGLINC)/heap.h \
	 $(DGLINC)/tree.h $(DGLINC)/type.h $(DGLINC)/helpers.h $(DGLINC)/graph_v1.h $(DGLINC)/graph_v2.h \
	 $(ARCH_INCDIR)/dgl.h lib

$(DGLINC)/avl.h: avl.h
	$(MKDIR) $(DGLINC)
	$(MKDIR) $(DGLINC_OLD)
	$(INSTALL) -m 644 avl.h $(DGLINC)/avl.h
	$(INSTALL) -m 644 avl.h $(DGLINC_OLD)/avl.h

$(DGLINC)/tavl.h: tavl.h
	$(INSTALL) -m 644 tavl.h $(DGLINC)/tavl.h
	$(INSTALL) -m 644 tavl.h $(DGLINC_OLD)/tavl.h

$(DGLINC)/heap.h: heap.h
	$(INSTALL) -m 644 heap.h $(DGLINC)/heap.h
	$(INSTALL) -m 644 heap.h $(DGLINC_OLD)/heap.h

$(DGLINC)/graph.h: graph.h
	$(INSTALL) -m 644 graph.h $(DGLINC)/graph.h
	$(INSTALL) -m 644 graph.h $(DGLINC_OLD)/graph.h

$(DGLINC)/tree.h: tree.h
	$(INSTALL) -m 644 tree.h $(DGLINC)/tree.h
	$(INSTALL) -m 644 tree.h $(DGLINC_OLD)/tree.h

$(DGLINC)/type.h: type.h
	$(INSTALL) -m 644 type.h $(DGLINC)/type.h
	$(INSTALL) -m 644 type.h $(DGLINC_OLD)/type.h

$(DGLINC)/graph_v1.h: graph_v1.h
	$(INSTALL) -m 644 graph_v1.h $(DGLINC)/graph_v1.h
	$(INSTALL) -m 644 graph_v1.h $(DGLINC_OLD)/graph_v1.h

$(DGLINC)/graph_v2.h: graph_v2.h
	$(INSTALL) -m 644 graph_v2.h $(DGLINC)/graph_v2.h
	$(INSTALL) -m 644 graph_v2.h $(DGLINC_OLD)/graph_v2.h

$(DGLINC)/helpers.h: helpers.h
	$(INSTALL) -m 644 helpers.h $(DGLINC)/helpers.h
	$(INSTALL) -m 644 helpers.h $(DGLINC_OLD)/helpers.h

$(ARCH_INCDIR)/dgl.h: dgl.h
	$(INSTALL) -m 644 dgl.h $(ARCH_INCDIR)/dgl.h
	$(INSTALL) -m 644 dgl.h $(ARCH_DISTDIR)/include/dgl.h

