
MODULE_TOPDIR = ../../..

LIB_NAME = $(DIG2_LIBNAME)

LIB_OBJS = \
    allocation.o  \
    angle.o  \
    box.o  \
    frmt.o \
    head.o \
    inside.o \
    line_dist.o \
    list.o \
    plus.o \
    plus_area.o  \
    plus_line.o \
    plus_node.o \
    plus_struct.o \
    poly.o \
    portable.o \
    spindex.o  \
    struct_alloc.o

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: $(ARCH_INCDIR)/portable.h lib $(OBJDIR)/test

#  Test the machine size and orientation for the Portable Library
$(ARCH_INCDIR)/portable.h:
	@test -d $(OBJDIR) || mkdir $(OBJDIR)
	$(CC) $(LDFLAGS) $(ALLINC) -o $(OBJDIR)/port_test port_test.c
	echo "/* Architecture: $(ARCH) */" > $(ARCH_INCDIR)/portable.h
	$(OBJDIR)/port_test >> $(ARCH_INCDIR)/portable.h
	
#  Test portable read/write functions
$(OBJDIR)/test: test.c 
	$(CC) $(LDFLAGS) $(ALLINC) -o $@ test.c $(DIG2LIB) $(GISLIB)
	cd $(OBJDIR); ./test


