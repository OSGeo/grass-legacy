# definitions for LINUX rim
RANLIB=ranlib
F77=g77
F2CLIB=/usr/lib/f2cmain.a -lf2c
EXTRA=time_date.o
LIBEXTRA=time_date.o
PILE = CC_f2c=$(CC) $(F77)
LIBRIM=librim.a
AR = ar rv
RAN = ranlib
# make rim libs (generic instructions)

#------------------------------------------------------------------#

# Files used by Rim and associates

# Main programs

# Module not used
UNUSED=ncodei.o  getloc.o

MAINS = rim.o rime.o lxtest.o helpgen.o

# Subroutines used by Rim and Rime

RIMSUBS = \
adddat.o amsg.o ascan.o ascchr.o ascdat.o \
asctxt.o atoi.o ator.o attadd.o attdel.o attget.o \
attnew.o attpag.o attput.o \
blkchg.o blkcln.o blkclr.o blkdef.o blkdsp.o blkdwn.o \
blkext.o blkloc.o blkmov.o blknxt.o blkup.o \
btadd.o btget.o btinit.o btlki.o btlkr.o \
btmove.o btput.o btrep.o btsert.o \
build.o change.o chgdat.o chgpsw.o chkatt.o \
chkfil.o chkrel.o chrasc.o chtype.o datjul.o dbdefn.o \
dbload.o dbopcl.o dbopen.o deldat.o deldup.o delete.o \
delrow.o dmsg.o dtfenc.o dtfspl.o dtor.o eq.o eqkeyw.o eqtok.o \
f1clo.o f1opn.o f2clo.o f2opn.o f3clo.o f3opn.o filch.o fmtdec.o \
getdat.o getkca.o gett.o gtsort.o \
htoi.o iexp.o imsg.o isect.o itoa.o itoh.o \
join.o joirel.o juldat.o kmpard.o kmpari.o kmparr.o kmpart.o kompxx.o \
lfind.o lkstr.o lnkadd.o lnkget.o lnkpag.o lnkput.o loadfm.o \
loadit.o loadmv.o locase.o locatt.o locboo.o loclnk.o locmac.o \
locprm.o locrel.o locvar.o lodele.o lodlnk.o lodpas.o lodrec.o \
lodrel.o lstrel.o lstrng.o lxcmnt.o lxdate.o lxedit.o lxfmt.o \
lxinit.o lxlenw.o lxsrec.o lxwrec.o macdef.o macexp.o motscn.o \
msg.o msgcmv.o ndigit.o ne.o nxtchr.o nxtchx.o \
nxtcrd.o nxttok.o parse.o parval.o pgbres.o pgbsto.o pgcexp.o \
pgcomp.o pgcprt.o pgeexp.o pgeprt.o pgexec.o pgstor.o \
pject.o prckey.o prjtup.o prmset.o prompt.o ptrs.o putdat.o putt.o \
reladd.o relget.o relpag.o relput.o remkey.o remlnk.o remrel.o \
report.o reuse.o rformt.o rimcmd.o rioclo.o rioin.o rioopn.o \
rioout.o ritoa.o rmclos.o rmcons.o rmdate.o rmhelp.o rminit.o \
rmlook.o rmpii.o rmqery.o rmres.o rmsav.o rmset.o rmshow.o \
rmtime.o rmtypt.o rmvarc.o rmzip.o rnamea.o rnamel.o rnamer.o \
roun.o rtoa.o rtod.o \
selatt.o select.o selout.o selput.o selrel.o selrpt.o selsrt.o \
selwhr.o setin.o setout.o sgtlnk.o sort.o strasc.o strmov.o \
subtrc.o swcon.o swcost.o swfilo.o swflfs.o swhart.o swhrtd.o swhrti.o \
swhrtr.o swicst.o swidcp.o swiicp.o swinpo.o swircp.o switcp.o swshel.o \
swsink.o swsmfl.o swsmvl.o swunlo.o swunvl.o swvacp.o swvlfs.o \
swvllo.o syscmd.o syscom.o sysdbg.o sysdbn.o sysdel.o sysexi.o \
sysini.o systrp.o tioclo.o tioin.o tioopn.o tioout.o tokdsp.o toktyp.o \
toled.o toler.o tuplrc.o typer.o undata.o undef.o union.o \
unload.o unpass.o upcase.o varadd.o warn.o whetol.o \
wheval.o whrnam.o xhibit.o zeroit.o zmove.o \
$(EXTRA)


#------------------------------------------------------------------#

# Subroutines in the user programmable interface library

LIBRIMSUBS = \
 adddat.o ascan.o ascchr.o ascdat.o asctxt.o atoi.o ator.o \
 attadd.o attdel.o attget.o attnew.o attpag.o attput.o \
 blkchg.o blkcln.o blkclr.o blkdef.o blkdsp.o blkdwn.o blkext.o \
 blkloc.o blkmov.o blknxt.o blkup.o btadd.o btget.o btinit.o \
 btlki.o btlkr.o btmove.o btput.o btrep.o btsert.o build.o \
 change.o chgdat.o chgpsw.o chkatt.o chkfil.o chkrel.o chrasc.o \
 chtype.o datjul.o dbdefn.o dbload.o dbopcl.o dbopen.o deldat.o \
 deldup.o delete.o delrow.o dtfenc.o dtfspl.o dtor.o eq.o eqkeyw.o \
 eqtok.o filch.o fmtdec.o f1clo.o f1opn.o f2clo.o f2opn.o \
 f3clo.o f3opn.o getdat.o getkca.o gett.o gtsort.o \
 htoi.o iexp.o isect.o itoa.o itoh.o join.o joirel.o juldat.o \
 kmpard.o kmpari.o kmparr.o kmpart.o kompxx.o lfind.o lkstr.o locase.o \
 locatt.o locboo.o locmac.o locprm.o locrel.o lodele.o lodpas.o \
 lodrec.o lodrel.o lstrel.o lstrng.o lxcmnt.o lxdate.o lxedit.o \
 lxfmt.o lxinit.o lxlenw.o lxsrec.o lxwrec.o macdef.o macexp.o \
 motscn.o ndigit.o ne.o nxtchr.o nxtchx.o nxttok.o \
 parse.o parval.o pilib.o pilibx.o pject.o prckey.o prjtup.o \
 ptrs.o putdat.o putt.o reladd.o relget.o relpag.o relput.o remkey.o \
 remrel.o reuse.o rformt.o rioclo.o rioin.o rioopn.o rioout.o \
 ritoa.o rmclos.o rmcons.o rmdate.o rminit.o rmlook.o rmpii.o \
 rmqery.o rmres.o rmsav.o rmset.o rmtime.o rmtypt.o rmvarc.o \
 rmxdel.o rmxget.o rmxlod.o rmxput.o rnamea.o rnamer.o roun.o \
 rtoa.o rtod.o selrel.o selsrt.o selwhr.o sort.o strasc.o strmov.o \
 subtrc.o swcon.o swcost.o swfilo.o swflfs.o swhart.o swhrtd.o \
 swhrti.o swhrtr.o swicst.o swidcp.o swiicp.o swinpo.o swircp.o \
 switcp.o swshel.o swsink.o swsmfl.o swsmvl.o swunlo.o swunvl.o \
 swvacp.o swvlfs.o swvllo.o syscmd.o syscom.o sysdbg.o sysdbn.o \
 sysdel.o tokdsp.o toktyp.o toled.o toler.o tuplrc.o typer.o \
 union.o upcase.o whetol.o wheval.o \
 whrnam.o xhibit.o zeroit.o zmove.o  \
 $(LIBEXTRA)


RXFILES = rmxget.o rmxput.o rmxlod.o rmxdel.o


#----------------------------------------------------------------------#

.SUFFIXES = .o .f .c
.f.o:	;$(PILE) -c -w $*.f
.c.o:	;cc  -c -w $*.c

all::
		make rim
		make rime
		make rimh
		make $(LIBRIM)
rim:		rim.o $(RIMSUBS)
	$(F77) -o rim rim.o $(RIMSUBS)

rime:		rime.o $(RIMSUBS)
	$(F77) -o rime rime.o $(RIMSUBS)

rimh:		rimh.o librim.a
	$(F77) -o rimh rimh.o librim.a

lxtest:		lxtest.o $(RIMSUBS)
	$(F77) -o lxtest lxtest.o $(RIMSUBS)

rimlib:		$(RIMSUBS)
		-rm rimlib
		$(AR)  rimlib $(RIMSUBS)
		ranlib rimlib
		
librim.a:	$(LIBRIMSUBS) $(LIBEXTRA)
		-rm librim.a
		$(AR) librim.a $(LIBRIMSUBS)
		ranlib librim.a

#-------------------------------------------------------------------#
