
MODULE_TOPDIR = ../../..

LIB_NAME = $(DIG2_LIBNAME)

LIB_OBJS = \
    allocation.o  \
    angle.o  \
    box.o  \
    file.o \
    frmt.o \
    head.o \
    inside.o \
    linecros.o \
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
    spindex_rw.o  \
    struct_alloc.o \
    update.o


include $(MODULE_TOPDIR)/include/Make/Lib.make

EXTRA_CLEAN_FILES = $(ARCH_INCDIR)/portable.h
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)
DEPENDENCIES =  $(GRASS_INCDIR)/vect/dig_defines.h \
                $(GRASS_INCDIR)/vect/dig_macros.h $(GRASS_INCDIR)/vect/dig_structs.h \
                $(GRASS_INCDIR)/vect/dig_externs.h $(GRASS_INCDIR)/vect/dig_globs.h $(GISDEP)

default: $(ARCH_INCDIR)/portable.h lib $(OBJDIR)/test

#  Test the machine size and orientation for the Portable Library
$(ARCH_INCDIR)/portable.h: $(DEPENDENCIES)
	@test -d $(OBJDIR) || mkdir $(OBJDIR)
	$(CC) $(LDFLAGS) $(VECT_CFLAGS) $(INC) $(VECT_INC) -o $(OBJDIR)/port_test port_test.c
	echo "/* Architecture: $(ARCH) */" > $(ARCH_INCDIR)/portable.h
	$(OBJDIR)/port_test >> $(ARCH_INCDIR)/portable.h
	
#  Test portable read/write functions
$(OBJDIR)/test: test.c $(DEPENDENCIES) 
	$(CC) $(LDFLAGS) $(VECT_CFLAGS) $(INC) $(VECT_INC) -o $@ test.c $(DIG2LIB) $(RTREELIB) \
				$(GISLIB) $(DATETIMELIB) $(MATHLIB) $(XDRLIB) 
	cd $(OBJDIR); LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(GISBASE)/lib ./test


