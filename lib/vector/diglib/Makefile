MODULE_TOPDIR = ../../..

EXTRA_LIBS=$(GISLIB) $(RTREELIB)

LIB_NAME = $(DIG2_LIBNAME)

LIB_OBJS = \
    allocation.o  \
    angle.o  \
    box.o  \
    cindex.o  \
    cindex_rw.o  \
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
    prune.o \
    spindex.o  \
    spindex_rw.o  \
    struct_alloc.o \
    type.o \
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
	@test -d $(OBJDIR) || $(MKDIR) $(OBJDIR)
	$(CC) $(LDFLAGS) $(COMPILE_FLAGS) $(VECT_CFLAGS) $(INC) $(VECT_INC) -o $(OBJDIR)/port_test port_test.c
	echo "/* Architecture: $(ARCH) */" > $(ARCH_INCDIR)/portable.h
	$(OBJDIR)/port_test >> $(ARCH_INCDIR)/portable.h
	echo "/* Architecture: $(ARCH) */" > $(ARCH_DISTDIR)/include/portable.h
	$(OBJDIR)/port_test >> $(ARCH_DISTDIR)/include/portable.h
	

#  Test portable read/write functions
$(OBJDIR)/test: test.c $(DEPENDENCIES) $(ARCH_LIB_OBJS)
	$(CC) $(LDFLAGS) $(COMPILE_FLAGS) $(VECT_CFLAGS) $(INC) $(VECT_INC) -o $@ test.c $(DIG2LIB) $(RTREELIB) \
				$(GISLIB) $(DATETIMELIB) $(MATHLIB) $(XDRLIB) 
	cd $(OBJDIR); $(LD_LIBRARY_PATH_VAR)="$($(LD_LIBRARY_PATH_VAR)):$(GISBASE)/lib" ./test; diff ./test.tmp ../test.ok


