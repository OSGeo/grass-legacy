# Makefile for the Xgen application generator
# Author: Kurt Buehler (buehlerk@cn.ecn.purdue.edu) as of Aug 5, 1990
#
# The objects......
OBJS =	addnode.o \
		alloctypes.o \
		button.o \
		buttonname.o \
		buttonpushed.o \
		check.o \
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
		popup.o \
		procmess.o \
		pulldown.o \
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
#
# The object and source for the Table widget
EXTRAWIDGETS = widgets/Table.o
EXTRASOURCES = widgets/Table.c
#
# The source files
SRCS =	addnode.c \
		alloctypes.c \
		button.c \
		buttonname.c \
		buttonpushed.c \
		check.c \
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
		popup.c \
		procmess.c \
		pulldown.c \
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
#
# The includes
INCLUDES =  xgen.h \
			resource.h \
			types.h

#
# Other stuff pertinent to a distribution
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

# 
# Uncomment and edit the following line to reflect the location 
# of the Motif libraries on your system, if they are not in /usr/lib
#
MOTIF = -L/usr/X/motif/lib
# 
# Uncomment and edit the following line to reflect the location 
# of the Motif includes on your system, if they are not in /usr/include/...
#
CFLAGS = -g -I/usr/include/Xm
YFLAGS = -d
LEXLIB = -ll
MATHLIB = -lm
XMLIB = -lXm
XTLIB = -lXtm
X11LIB = -lX11
XLIBS = $(MOTIF) $(XMLIB) $(XTLIB) $(X11LIB) 
LIBS = $(LEXLIB) $(XLIBS) $(MATHLIB)

#
# Deal with the following suffixes
.SUFFIXES: .o .c .y .l

#
# Main target
xgen: $(OBJS) $(EXTRAWIDGETS)
	rm -f xgen
	cc $(OBJS) $(EXTRAWIDGETS) -o $@ $(LIBS)

#
# Dependencies
$(OBJS): xgen.h types.h 
grammar.c: grammar.y resource.h xgen.h 
grammar.h: 
	mv y.tab.h grammar.h
lex.c: lex.l grammar.c grammar.h
resource.o: resource.h
$(EXTRAWIDGETS): $(EXTRASOURCES)
	(cd widgets; make Table.o)

# 
# RCS check in 
checkin:
	ci -m"automatic check in: `date`" -f $(SRCS) $(INCLUDES)

# 
# RCS check out 
checkout:
	co -l $(SRCS) $(INCLUDES)

#
# Clean up
clean:
	@/bin/rm -rf lex.c grammar.c grammar.h *.o widgets/Table.o

#
# Inventory
count:
	@echo "Source Code Inventory:"
	@echo "   Lines   Chars Filename"
	@echo "   -----   ----- --------"
	@wc -lc $(SRCS)

#
# make a new distribution
dist:
	tar cvf xgen.tar $(SRCS) $(INCLUDES) $(OTHERSTUFF) $(EXTRASOURCES)
	compress xgen.tar
	mv xgen.tar.Z xgen.`date | awk '{printf "%s.%d\n" , $$2, $$3}' -`.tar.Z

#
# make tags
tags: $(SRCS)
	ctags $(SRCS)
