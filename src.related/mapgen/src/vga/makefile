OS                  = SYSV
COMPILE_FLAGS       = 
LDFLAGS             = 
MATHLIB             = -lm
TERMLIB             = -ltermlib
CLEAR               = no
AR                  = ar rc $@ `lorder $(OBJ) | tsort`
CC                  = cc
LD                  = cc
###########################################################
CFLAGS      = $(COMPILE_FLAGS) -D$(OS) $(EXTRA_CFLAGS)

GRAPH = 

OBJ =  \
	Box.o\
	Can_do.o\
	Color.o\
	Cont_abs.o \
	Cont_rel.o \
	Draw_line.o\
	Erase.o\
	Graph_Clse.o\
	Graph_Set.o\
	Mouse.o \
	Move_abs.o \
	Move_rel.o \
	Panel.o\
	Plydts_abs.o \
	Plydts_rel.o \
	Plylne_abs.o \
	Plylne_rel.o \
	Polygn_abs.o\
	Polygn_rel.o\
	Raster_chr.o\
	Raster_int.o\
	Reset_clr.o \
	Returns.o\
	Text.o


vgalib.a: $(OBJ)
	$(AR)

###########################################################
# the tags file created by "make ctags" is great for
# editting to look for function definitions
# vi -t <function name>

ctags:
	ctags *.[ch]
	sed 's/\?/\//g' tags > tags.tmp
	mv tags.tmp tags

# this next rule builds .s files from .c files preserving all the CFLAGS
# to make obj.s simply enter "make obj.s"

.c.s:
	$(CC) $(CFLAGS) -S $<

# this next rule builds .o files from .c files preserving all the CFLAGS

.c.o:
	$(CC) $(CFLAGS) -c $<
