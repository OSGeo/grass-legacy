MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Module.make

PGM2 = r.mapcalc
PGM3 = r3.mapcalc

COMMON = \
	$(OBJDIR)/y.tab.o \
	$(OBJDIR)/lex.yy.o \
	\
	$(OBJDIR)/evaluate.o \
	$(OBJDIR)/expression.o \
	$(OBJDIR)/function.o \
	$(OBJDIR)/check.o \
	$(OBJDIR)/main.o \
	\
	$(OBJDIR)/xabs.o \
	$(OBJDIR)/xadd.o \
	$(OBJDIR)/xand.o \
	$(OBJDIR)/xatan.o \
	$(OBJDIR)/xcos.o \
	$(OBJDIR)/xdiv.o \
	$(OBJDIR)/xdouble.o \
	$(OBJDIR)/xeq.o \
	$(OBJDIR)/xeval.o \
	$(OBJDIR)/xexp.o \
	$(OBJDIR)/xfloat.o \
	$(OBJDIR)/xge.o \
	$(OBJDIR)/xgraph.o \
	$(OBJDIR)/xgt.o \
	$(OBJDIR)/xif.o \
	$(OBJDIR)/xint.o \
	$(OBJDIR)/xisnull.o \
	$(OBJDIR)/xle.o \
	$(OBJDIR)/xlog.o \
	$(OBJDIR)/xlt.o \
	$(OBJDIR)/xmax.o \
	$(OBJDIR)/xmedian.o \
	$(OBJDIR)/xmin.o \
	$(OBJDIR)/xmod.o \
	$(OBJDIR)/xmode.o \
	$(OBJDIR)/xmul.o \
	$(OBJDIR)/xne.o \
	$(OBJDIR)/xneg.o \
	$(OBJDIR)/xnot.o \
	$(OBJDIR)/xnull.o \
	$(OBJDIR)/xor.o \
	$(OBJDIR)/xpow.o \
	$(OBJDIR)/xrand.o \
	$(OBJDIR)/xround.o \
	$(OBJDIR)/xrowcol.o \
	$(OBJDIR)/xsin.o \
	$(OBJDIR)/xsqrt.o \
	$(OBJDIR)/xsub.o \
	$(OBJDIR)/xtan.o

LIST2 =	$(COMMON) \
	$(OBJDIR)/map.o \
	$(OBJDIR)/xcoor.o \
	$(OBJDIR)/xres.o

LIST3 =	$(COMMON) \
	$(OBJDIR)/map3.o \
	$(OBJDIR)/xcoor3.o \
	$(OBJDIR)/xres3.o

EXTRA_CFLAGS = $(READLINEINCPATH)
LIBES2 = $(GISLIB) $(BTREELIB) $(ROWIOLIB) $(READLINELIBPATH) $(READLINELIB) $(HISTORYLIB)
LIBES3 = $(GISLIB) $(BTREELIB) $(G3DLIB) $(READLINELIBPATH) $(READLINELIB) $(HISTORYLIB)

default: y.tab.c y.tab.h lex.yy.c $(BIN_CMD)/$(PGM2) $(BIN_CMD)/$(PGM3) htmlcmd

$(BIN_CMD)/$(PGM2): $(LIST2) $(DEPLIBS2)
	$(CC) $(LDFLAGS) -o $@ $(LIST2) $(LIBES2) $(MATHLIB) $(XDRLIB)
	@test -x $(BIN_CMD)/$(PGM2) || ln $(ETC)/front.end $(BIN)/$(PGM2)
	
$(BIN_CMD)/$(PGM3): $(LIST3) $(DEPLIBS3)
	$(CC) $(LDFLAGS) -o $@ $(LIST3) $(LIBES3) $(MATHLIB) $(XDRLIB)
	@test -x $(BIN_CMD)/$(PGM3) || ln $(ETC)/front.end $(BIN)/$(PGM3)

y.tab.c y.tab.h: mapcalc.y
	$(YACC) -b $(OBJDIR)/y -d mapcalc.y
	$(MKDIR) $(OBJDIR)
	$(CC) $(LDFLAGS) $(INC) $(XTRA_LDFLAGS) -o $(OBJDIR)/$*.o -c $*.c

lex.yy.c: mapcalc.l y.tab.h
	$(LEX) -t mapcalc.l > $@
	$(CC) $(LDFLAGS) $(INC) $(XTRA_LDFLAGS) -o $(OBJDIR)/$*.o -c $*.c

#clean:
#	rm -f y.tab.c y.tab.h lex.yy.c y.output lex.backup *~

htmlcmd:
	$(MKDIR) $(GISBASE)/docs/html
	$(INSTALL) -m 644 $(PGM2).html $(GISBASE)/docs/html/
	$(INSTALL) -m 644 $(PGM3).html $(GISBASE)/docs/html/
