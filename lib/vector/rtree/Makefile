
MODULE_TOPDIR = ../../..

LIB_NAME = $(RTREE_LIBNAME)

LIB_OBJS = card.o \
	gammavol.o \
	index.o \
	node.o \
	rect.o \
	sphvol.o \
	split_q.o 

include $(MODULE_TOPDIR)/include/Make/Lib.make

RTLINC = $(ARCH_INCDIR)/rtree/

default: $(RTLINC)/card.h $(RTLINC)/index.h $(RTLINC)/split_l.h $(RTLINC)/split_q.h \
	$(ARCH_INCDIR)/rtree.h lib $(OBJDIR)/test

$(RTLINC)/card.h: card.h
	mkdir -p $(RTLINC)
	cp card.h $(RTLINC)/card.h

$(RTLINC)/index.h: index.h
	cp index.h $(RTLINC)/index.h

$(RTLINC)/split_l.h: split_l.h
	cp split_l.h $(RTLINC)/split_l.h

# not used
$(RTLINC)/split_q.h: split_q.h
	cp split_q.h $(RTLINC)/split_q.h

$(ARCH_INCDIR)/rtree.h: rtree.h
	cp rtree.h $(ARCH_INCDIR)/rtree.h

$(OBJDIR)/test: test.c
	$(CC) $(LDFLAGS) $(ALLINC) -o $@ test.c $(RTREELIB) -lm
	cd $(OBJDIR); ./test


