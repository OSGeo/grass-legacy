MOTIF = -L/usr/local/X11/motif/lib
CFLAGS = -g -I/usr/local/X11/motif/include 
OBJS =	addnode.o \
		alloctypes.o \
		button.o \
		buttonname.o \
		buttonpushed.o \
		clear.o \
		cmdboard.o \
		command.o \
		control.o \
		createobject.o \
		createshell.o \
		destroyshell.o \
		docapture.o \
		dodup.o \
		doerror.o \
		doexec.o \
		dojobs.o \
		doset.o \
		error.o \
		exit.o \
		expand.o \
		expandvar.o \
		fileselect.o \
		font.o \
		grammar.o \
		help.o \
		index.o \
		initialshell.o \
		label.o \
		lex.o \
		list.o \
		listchanged.o \
		listdata.o \
		main.o \
		menu.o \
		message.o \
		msgboard.o \
		objectargs.o \
		objectval.o \
		parsecom.o \
		parsepath.o \
		popup.o \
		procmess.o \
		resource.o \
		separator.o \
		signal.o \
		slider.o \
		strpart.o \
		table.o \
		textentry.o \
		toggle.o \
		togglechanged.o \
		toggledata.o \
		unique.o 
EXTRAWIDGETS = widgets/Table.o
EXTRASOURCES = widgets/Table.c
SRCS =	addnode.c \
		alloctypes.c \
		button.c \
		buttonname.c \
		buttonpushed.c \
		clear.c \
		cmdboard.c \
		command.c \
		control.c \
		createobject.c \
		createshell.c \
		destroyshell.c \
		docapture.c \
		dodup.c \
		doerror.c \
		doexec.c \
		dojobs.c \
		doset.c \
		error.c \
		exit.c \
		expand.c \
		expandvar.c \
		fileselect.c \
		font.c \
		grammar.y \
		help.c \
		index.c \
		initialshell.c \
		label.c \
		lex.l \
		list.c \
		listchanged.c \
		listdata.c \
		main.c \
		menu.c \
		message.c \
		msgboard.c \
		objectargs.c \
		objectval.c \
		parsecom.c \
		parsepath.c \
		popup.c \
		procmess.c \
		resource.c \
		separator.c \
		signal.c \
		slider.c \
		strpart.c \
		table.c \
		textentry.c \
		toggle.c \
		togglechanged.c \
		toggledata.c \
		unique.c
INCLUDES =  xgen.h \
			resource.h \
			types.h

OTHERSTUFF = Makefile \
			 EXAMPLE_SCRIPT \
			 dp.xbm \
			 gnu.xbm \
			 mtn.xbm \
			 widgets/Table.h \
			 widgets/TableP.h \
			 widgets/README \
			 widgets/Makefile \
			 widgets/test.c

EXE = xgen_SUN3 xgen_SUN4

LEXLIB = -ll
MATHLIB = -lm
XMLIB = -lXm
XTLIB = -lXt
X11LIB = -lX11
XLIBS = $(MOTIF) $(XMLIB) $(XTLIB) $(X11LIB) 
LIBS = $(LEXLIB) $(XLIBS) $(MATHLIB)

xgen_$(ARC): $(OBJS) $(EXTRAWIDGETS)
	rm -f xgen
	cc $(OBJS) $(EXTRAWIDGETS) -o $@ $(LIBS)
	ln -s $@ xgen

grammar.c: grammar.y resource.h xgen.h
	yacc -d grammar.y
	@mv y.tab.c grammar.c
	@mv y.tab.h grammar.h

lex.c: lex.l grammar.c grammar.h
	lex lex.l
	@mv lex.yy.c lex.c

checkin:
	ci -m"automatic check in: `date`" -f $(SRCS) $(INCLUDES)

checkout:
	co -l $(SRCS) $(INCLUDES)

clean:
	@/bin/rm -rf lex.c grammar.c grammar.h *.o xgen 

count:
	@echo "Source Code Inventory:"
	@echo "   Lines   Chars Filename"
	@echo "   -----   ----- --------"
	@wc -lc $(SRCS)

dist:
	tar cvf xgen.tar $(SRCS) $(INCLUDES) $(OTHERSTUFF) $(EXTRASOURCES) $(EXE)
	compress xgen.tar

tags: $(SRCS)
	ctags $(SRCS)

$(OBJS): xgen.h types.h 
resource.o: resource.h

$(EXTRAWIDGETS): $(EXTRASOURCES)
	(cd widgets; make Table.o)
