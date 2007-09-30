MODULE_TOPDIR = ../../..

EXTRA_LIBS=$(GISLIB) $(VECTLIB) $(G3DLIB)

LIB_NAME = $(MANAGE_LIBNAME)

LIB_OBJS = add_elem.o ask.o copyfile.o do_copy.o do_list.o do_remove.o \
           do_rename.o empty.o find.o get_len.o menu.o read_list.o \
           show_elem.o sighold.o

LOCAL_HEADERS = $(wildcard *.h) ../list.h

EXTRA_INC = -I.. $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)
DEPENDENCIES = $(GISDEP) $(VECTDEP) $(G3DDEP)

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: stlib $(ETC)/element_list

$(ETC)/element_list: Element_List
	if [ ! -d $(ETC) ]; then $(MKDIR) $(ETC); fi
	$(INSTALL_DATA) $? $@
