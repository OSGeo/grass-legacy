MODULE_TOPDIR = ../..

PGM = r.mapcalc

CMD_OBJS = y.tab.o \
	lex.yy.o \
	\
	expression.o \
	function.o \
	check.o \
	main.o \
	evaluate.o \
	map.o \
	\
	xabs.o \
	xadd.o \
	xand.o \
	xatan.o \
	xcoor.o \
	xcos.o \
	xdiv.o \
	xdouble.o \
	xeq.o \
	xeval.o \
	xexp.o \
	xfloat.o \
	xge.o \
	xgraph.o \
	xgt.o \
	xif.o \
	xint.o \
	xisnull.o \
	xle.o \
	xlog.o \
	xlt.o \
	xmax.o \
	xmedian.o \
	xmin.o \
	xmod.o \
	xmode.o \
	xmul.o \
	xne.o \
	xneg.o \
	xnot.o \
	xnull.o \
	xor.o \
	xpow.o \
	xrand.o \
	xres.o \
	xround.o \
	xrowcol.o \
	xsin.o \
	xsqrt.o \
	xsub.o \
	xtan.o

EXTRA_CFLAGS = $(READLINEINCPATH)
LIBES = $(GISLIB) $(BTREELIB) $(ROWIOLIB) $(READLINELIB) $(HISTORYLIB)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: y.tab.c y.tab.h lex.yy.c cmd

y.tab.c y.tab.h: mapcalc.y
	$(YACC) -d mapcalc.y
	\mkdir -p $(OBJDIR)
	$(CC) $(LDFLAGS) $(INC) $(XTRA_LDFLAGS) -o $(OBJDIR)/$*.o -c $*.c

lex.yy.c: mapcalc.l y.tab.h
	$(LEX) -t mapcalc.l > $@
	$(CC) $(LDFLAGS) $(INC) $(XTRA_LDFLAGS) -o $(OBJDIR)/$*.o -c $*.c

#clean:
#	rm -f y.tab.c y.tab.h lex.yy.c y.output lex.backup *~
