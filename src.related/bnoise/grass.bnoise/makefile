GMAKE=/home/grass3/src/CMD/Gmake
# SUN micro. no floating point processor
# set these definitions according to your system requirements

GIS                 = /home/grass3
GISDBASE            = /home/grass3/data
UNIX_BIN            = /usr/local/bin
DEFAULT_LOCATION    = spearfish

OS                  = BERK
COMPILE_FLAGS       = -O
DIGIT_FLAGS         =
LDFLAGS             = -s
TERMLIB             = -ltermlib
MATHLIB             = -lm
CLEAR               = ok
AR                  = ar ruv $@ $?; ranlib $@
###########################################################
CFLAGS      = $(COMPILE_FLAGS) -I$(LIBDIR) -D$(OS) $(EXTRA_CFLAGS)
GMAKE       = $(GIS)/src/CMD/Gmake
MAKEALL     = set - *; for d do test -f $$d/Gmakefile && $(GMAKE) $$d; done; exit 0
MANROFF     = tbl -TX $(GIS)/src/man.help/man.header $? | nroff -Tlp | col -b > $@
MAN1        = $(GIS)/man/1
MAN2        = $(GIS)/man/2
HELP        = $(GIS)/man/help

BIN         = $(GIS)/bin
ETC         = $(GIS)/etc
SRC         = $(GIS)/src
LIBDIR      = $(GIS)/src/libes
GISLIB      = $(LIBDIR)/libgis.a
VASKLIB     = $(LIBDIR)/libvask.a
LOCKLIB     = $(LIBDIR)/liblock.a
IMAGERYLIB  = $(LIBDIR)/libI.a
SEGMENTLIB  = $(LIBDIR)/libsegment.a
DLGLIB      = $(LIBDIR)/libdlg.a
CURSES      = -lcurses $(TERMLIB)
VASK        = $(VASKLIB) $(CURSES)
RASTERLIB   = $(SRC)/D/libes/rasterlib.a
DISPLAYLIB  = $(SRC)/D/libes/displaylib.a
D_LIB       = $(SRC)/D/libes/Dlib.a
###########################################################
# %W%  %G%
PGM=grass.bnoise

LIST = ask_weapon.o add_weap.o choose_weap.o editor.o read_weapon.o \
       show_weapons.o write_weapon.o add_targ.o choose_targ.o \
	   read_target.o show_targets.o write_target.o save_param.o \
	   add_fp.o choose_fp.o read_fp.o show_fp.o write_fp.o get_answer.o \
	   run_Bnoise.o write7.o finish.o new.o remove_param.o


all: $(BIN)/grass.bnoise help

$(BIN)/grass.bnoise: $(LIST) $(GISLIB)
	cc $(LDFLAGS) $(LIST) $(GISLIB) $(TERMLIB) $(VASK) -o $@
	cp new.table $(GIS)/etc/gun.table
	cp TAPE20.DAT $(GIS)/etc/TAPE20.DAT


editor.o:  edit.h

add_fp.o:    edit.h

add_targ.o:  edit.h

add_weap.o:  edit.h

ask_weapon.o:  edit.h

choose_fp.o:    edit.h

choose_targ.o:  edit.h

choose_weap.o:  edit.h

get_answer.o:    edit.h

new.o:    edit.h

read_fp.o:    edit.h

read_target.o:  edit.h

read_weapon.o:  edit.h

remove_param.o:  edit.h

run_Bnoise.o:  edit.h

save_param.o:  edit.h

show_fp.o:    edit.h

show_targets.o:  edit.h

show_weapons.o:  edit.h

write7.o:    edit.h

write_fp.o:    edit.h

write_target.o:  edit.h

write_weapon.o:  edit.h


help:
	cp HELP $(HELP)/$(PGM); chmod +r $(HELP)/$(PGM)
	cp MAN.cat $(MAN1)/$(PGM); chmod +r $(MAN1)/$(PGM)

MAN.cat: MAN.nroff
	$(MANROFF)
$(GISLIB): #
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
	cc $(CFLAGS) -S $<
