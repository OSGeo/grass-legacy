GMAKE=/files/grass3.1/src/CMD/Gmake
# set these definitions according to your system requirements

GIS                 = /files/grass3.1
GISDBASE            = /files/grass3.1/data
UNIX_BIN            = /files/grass3.1/local/bin
DEFAULT_LOCATION    = spearfish

#OS                  = SYSV
OS                  = BERK
COMPILE_FLAGS       = -O
LDFLAGS             = -s
DIGIT_FLAGS         =
#DIGIT_FLAGS         = -DATT
#DIGIT_FLAGS         = -DMASSCOMP
MATHLIB             = -lm
TERMLIB             = -ltermlib
CURSES              = -lcurses $(TERMLIB)
#CURSES              = -loldcurses $(TERMLIB)
CLEAR               = ok
#CLEAR               = no
AR                  = ar ruv $@ $?; ranlib $@
#AR                  = ar rc $@ `lorder $(OBJ) | tsort`
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
#  %W%  %G%
COORCNVLIB=$(LIBDIR)/libcoorcnv.a

OFILES = \
	main.o\
	a_n_distance.o\
        draw_ARC.o\
        draw_NODE.o\
	draw_SEGMENT.o\
	draw_PLUS.o\
	clip.o\
	nearest_node.o\
	pt_on_seg.o\
	arc_in_WIND.o\
	pt_in_WIND.o\
	choose_arc.o\
	chosen_arc.o\
	draw_seg.o\
	dr_me_pr_ex.o\
	erase_o_g.o\
	erase_pp.o\
	erase_lm.o\
	only_1_arc.o\
	arc_is_dot.o\
	nearest_seg.o\
	distance.o\
	dist_seg.o\
	dist_SEGMENT.o\
	dist_ARC.o\
	valid_seg.o\
	valid_pt.o\
	redraw_init.o\
	draw_WIND.o\
	erase_node.o\
	N_dr_me_pr_e.o\
	dist_seg_N.o\
	term.o\
	rd_dg_bn_hd.o\
	dig_asc_file.o\
	prompt4asc.o\
	dig_bin_file.o\
	prompt4bin.o\
	wr_dg_hd_bn.o\
	wr_dg_bn_pts.o\
	A.o\
	B.o\
	C.o\
	D.o\
	E.o\
	F.o\
	G.o\
	H.o\
	I.o\
	J.o\
	K.o\
	L.o\
	M.o\
	N.o\
	O.o\
	P.o\
	R.o\
	T.o\
	U.o\
	V.o\
	X.o\
	Y.o\
	Z.o\
	START.o\
	setup.o\
	END.o\
	erase4over.o\
	error_msg.o

d.distance: $(BIN)/d.distance

$(BIN)/d.distance: $(OFILES) $(DISPLAYLIB) $(RASTERLIB) $(COORCNVLIB) $(GISLIB) $(DIGLIB) $(MATHLIB) $(VASKLIB) $(CURSES)
	cc -o $@ $(LDFLAGS) $(OFILES) $(DISPLAYLIB) $(RASTERLIB) $(COORCNVLIB) $(GISLIB) $(DIGLIB) $(MATHLIB) $(VASKLIB) $(CURSES)

$(DISPLAYLIB): #
$(RASTERLIB): #
$(COORCNVLIB): #
$(GISLIB): #
$(DIGLIB): #
$(MATHLIB): #
$(VASKLIB): #
$(CURSES): #
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
