MODULE_TOPDIR = ../../..

EXTRA_LIBS=$(GISLIB) $(VECTLIB)

LIB_NAME = $(MANAGE_LIBNAME)

LIB_OBJS = add_elem.o ask.o copyfile.o do_copy.o do_list.o do_remove.o \
           do_rename.o empty.o find.o get_len.o menu.o read_list.o \
           show_elem.o sighold.o

EXTRA_INC = -I ../ $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)
DEPENDENCIES = $(GISDEP) $(VECTDEP)

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib $(ETC)/element_list $(ETC)/copy

$(ETC)/element_list: Element_List
	if [ ! -d $(ETC) ]; then mkdir $(ETC); fi
	cp $? $@ ; chmod 0444 $@ ;

$(ETC)/copy: copy.sh
	cp $? $@ ; chmod 0755 $@ ;
