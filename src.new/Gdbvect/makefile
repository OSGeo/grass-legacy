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

#COMPILE_FLAGS = -g -DDEBUGIO
COMPILE_FLAGS = -g
LDFLAGS = -g
FFLAGS =  -g
#FFLAGS = 
MYLIBS = $(SRC).contrib/rim/extra.src/* $(SRC).contrib/rim/librim.a 
EXTRA_CFLAGS = -I$(SRC)/mapdev/lib

OBJFILES = Gdbvect.o dbio.o parser.o top_level.o misc.o make_init.o \
	make_add.o make_done.o use_table.o fill_buf.o f_table.o \
	save_finfo.o rim_type.o rim_text_len.o res_split_f.o \
	show.o retr_buf.o input_file.o output_file.o get_max_seq.o \
	vect_map.o input_stack.o remove.o tables.o field_info.o \
	s_table.o list.o backup.o field_offset.o field_val.o \
	fill_field.o clear_value.o add_change.o val_from_str.o delete.o \
	my_G_yes.o find_query.o fill_value.o val_to_str.o print.o \
	read_vect.o strip_fields.o map.o maps.o m_table.o get_max_map.o \
	type_to_char.o site_list.o

all: Gdbvect $(MAN2)/Gdbvect $(HELP)/Gdbvect

Gdbvect: $(OBJFILES)
	f77 $(LDFLAGS) -o $@ $(FFLAGS) $(OBJFILES) $(MYLIBS) $(GISLIB) $(DIGLIB)


$(MAN2)/Gdbvect: Gdbvect.man
	$(MANROFF)

$(HELP)/Gdbvect: Gdbvect.help
	cp Gdbvect.help $@

dbio.o: globals.h parse.h rim.h
parser.o: globals.h parse.h opcode.h
top_level.o: globals.h opcode.h parse.h
misc.o: globals.h
make_init.o: globals.h make.h rim.h 
make_add.o: globals.h make.h
make_done.o: globals.h parse.h make.h
use_table.o: globals.h 
fill_buf.o: 
fil_f_table.o: 
fil_s_table.o: 
save_finfo.o: globals.h
rim_type.o: make.h
rim_text_length.o: globals.h make.h
res_split_f.o: globals.h
retr_buf.o:
show.o: globals.h rim.h
input_file.o: globals.h
output_file.o: globals.h
vect_map.o: globals.h vect.h
input_stack.o: globals.h
remove.o: globals.h
tables.o: globals.h
field_info.o: globals.h rim.h
backup.o: globals.h
list.o: globals.h
field_offset.o: globals.h rim.h make.h
field_val.o: globals.h rim.h make.h
fill_field.o: globals.h rim.h make.h
fill_value.o: globals.h rim.h make.h
clear_value.o: globals.h rim.h make.h
add_change.o: globals.h rim.h make.h
val_from_str.o: globals.h rim.h make.h
val_to_str.o: globals.h rim.h make.h
delete.o: globals.h rim.h
my_G_yes.o:
find_query.o: globals.h rim.h
print.o: globals.h rim.h make.h
read_vect.o: globals.h rim.h make.h vect.h
map.o: globals.h rim.h
maps.o: globals.h rim.h
type_to_char.o: vect.h
site_list.o: globals.h rim.h
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
