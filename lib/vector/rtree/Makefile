MODULE_TOPDIR = ../../..

LIB_NAME = $(RTREE_LIBNAME)

LIB_OBJS = card.o \
	gammavol.o \
	index.o \
	node.o \
	rect.o \
	split_q.o 

include $(MODULE_TOPDIR)/include/Make/Lib.make

RTLINC = $(ARCH_INCDIR)/rtree/
RTLINC_OLD = $(ARCH_DISTDIR)/include/rtree/

default: $(RTLINC)/card.h $(RTLINC)/index.h $(RTLINC)/split_l.h $(RTLINC)/split_q.h \
	$(ARCH_INCDIR)/rtree.h lib

$(RTLINC)/card.h: card.h
	$(MKDIR) $(RTLINC)
	$(MKDIR) $(RTLINC_OLD)
	$(INSTALL) -m 644 card.h $(RTLINC)/card.h
	$(INSTALL) -m 644 card.h $(RTLINC_OLD)/card.h

$(RTLINC)/index.h: index.h
	$(INSTALL) -m 644 index.h $(RTLINC)/index.h
	$(INSTALL) -m 644 index.h $(RTLINC_OLD)/index.h

$(RTLINC)/split_l.h: split_l.h
	$(INSTALL) -m 644 split_l.h $(RTLINC)/split_l.h
	$(INSTALL) -m 644 split_l.h $(RTLINC_OLD)/split_l.h

# not used
$(RTLINC)/split_q.h: split_q.h
	$(INSTALL) -m 644 split_q.h $(RTLINC)/split_q.h
	$(INSTALL) -m 644 split_q.h $(RTLINC_OLD)/split_q.h

$(ARCH_INCDIR)/rtree.h: rtree.h
	$(INSTALL) -m 644 rtree.h $(ARCH_INCDIR)/rtree.h
	$(INSTALL) -m 644 rtree.h $(ARCH_DISTDIR)/include/rtree.h

$(OBJDIR)/test: test.c
	$(CC) $(LDFLAGS) $(INC) -o $@ test.c $(RTREELIB) -lm
	cd $(OBJDIR); ./test

