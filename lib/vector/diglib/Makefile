
MODULE_TOPDIR = ../../..

LIB_NAME = $(DIG2_LIBNAME)

LIB_OBJS = \
    allocation.o  \
    area.o \
    bound_box.o  \
    calc_angl.o  \
    check_node.o  \
    chk_inside.o \
    clock.o \
    cont_lines.o \
    debugf.o \
    file_checks.o \
    find_area.o \
    format.o \
    frmt.o \
    isle.o \
    linecros.o \
    line_dist.o \
    load_plus.o \
    map_to_head.o  \
    memory_io.o \
    node_op.o  \
    point_t_line.o \
    point_t_area.o \
    prune.o  \
    rw_ascii.o  \
    struct_alloc.o  \
    struct_io.o  \
    struct_copy.o \
    unit_conv.o \
    write_file.o \
    portable.o \
    x_struct_io.o 

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
	$(CC) $(LDFLAGS) $(ALLINC) -static -o $@ test.c $(DIG2LIB)
	cd $(OBJDIR); ./test


