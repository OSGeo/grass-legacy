GMAKE=/home/grass3.1/src/CMD/Gmake
# SUN
# set these definitions according to your system requirements

GIS                 = /home/grass3.1
GISDBASE            = /home/grass3.1/data
UNIX_BIN            = /usr/local/bin
DEFAULT_LOCATION    = spearfish

OS                  = BERK
COMPILE_FLAGS       = -O
DIGIT_FLAGS         =
LDFLAGS             = -s
TERMLIB             = -ltermlib
CURSES              = -lcurses $(TERMLIB)
MATHLIB             = -lm
CLEAR               = ok
AR                  = ar ruv $@ $?; ranlib $@
###########################################################
CFLAGS      = $(COMPILE_FLAGS) -I$(LIBDIR) -D$(OS) $(EXTRA_CFLAGS)
GMAKE       = $(GIS)/src/CMD/Gmake
MAKEALL     = $(GIS)/src/CMD/Makeall $(GMAKE)
MANROFF     = tbl -TX $(GIS)/src/man.help/man.header $? | nroff -Tlp | col -b > $@
MAN1        = $(GIS)/man/1
MAN2        = $(GIS)/man/2
HELP        = $(GIS)/man/help

BIN         = $(GIS)/bin
ETC         = $(GIS)/etc
SRC         = $(GIS)/src
TXT         = $(GIS)/txt
LIBDIR      = $(GIS)/src/libes
GISLIB      = $(LIBDIR)/libgis.a
VASKLIB     = $(LIBDIR)/libvask.a
LOCKLIB     = $(LIBDIR)/liblock.a
IMAGERYLIB  = $(LIBDIR)/libI.a
ROWIOLIB    = $(LIBDIR)/librowio.a
SEGMENTLIB  = $(LIBDIR)/libsegment.a
DLGLIB      = $(LIBDIR)/libdlg.a
VASK        = $(VASKLIB) $(CURSES)
RASTERLIB   = $(SRC)/D/libes/rasterlib.a
DISPLAYLIB  = $(SRC)/D/libes/displaylib.a
D_LIB       = $(SRC)/D/libes/Dlib.a

DIGLIB      = $(SRC)/mapdev/lib/libdig.a
DIG_ATTLIB  = $(LIBDIR)/libdig_atts.a
###########################################################

MYINCLUDE = ../Gdbsites
#EXTRA_CFLAGS = -g -DDEBUGIO
EXTRA_CFLAGS =  -I$(MYINCLUDE) -DDBSITES
#FFLAGS =  -g
FFLAGS = 
MYLIBS = $(SRC).contrib/rim/extra.src/* $(SRC).contrib/rim/librim.a 
GPTH = ../Gdbsites/

EXTRNOBJS = clear_value.o fill_value.o fill_field.o add_change.o find_query.o \
	retr_buf.o fill_buf.o dbio.o val_from_str.o backup.o misc.o \
	field_info.o rim_text_len.o ret_s_table.o fil_s_table.o ret_f_table.o \
	fil_f_table.o list.o field_val.o make_init.o make_add.o make_done.o \
	rim_type.o save_finfo.o res_split_f.o field_offset.o delete.o \
	print.o my_G_yes.o val_to_str.o site_list.o read_sites.o strip_fields.o


OBJFILES = dbsites.o layout.o main_menu.o mgmt_menu.o fnd_qry.o add_chng_del.o \
	view_show.o site_pgms.o top_lvl.o mgmt_pgms.o parser.o do_open.o \
	make_new.o

all: $(BIN)/dbsites $(MAN1)/dbsites $(HELP)/dbsites

$(BIN)/dbsites: $(EXTRNOBJS) $(OBJFILES)
	f77 $(LDFLAGS) -o $@ $(FFLAGS) $(OBJFILES) $(EXTRNOBJS) \
	     $(MYLIBS) $(GISLIB) $(VASK)

$(EXTRNOBJS): $(GPTH)$(@:.o=.c) $(GPTH)globals.h $(GPTH)make.h $(GPTH)parse.h \
		  $(GPTH)rim.h
	$(CC) $(CFLAGS) -c $(GPTH)$(@:.o=.c) -o $@

$(MAN1)/dbsites: Man.pages
	$(MANROFF)

$(HELP)/dbsites: Help
	cp Help $@

###########################################################
# the tags file created by "make ctags" is great for
# editting to look for function definitions
# vi -t <function name>

ctags:
	ctags *.[ch]
	sed 's/\?/\//g' tags > tags.tmp
	mv tags.tmp tags

# this next rule builds .s files from .o files preserving all the CFLAGS
# to make obj.s simply enter "make obj.s"

.c.s:
	$(CC) $(CFLAGS) -S $<
