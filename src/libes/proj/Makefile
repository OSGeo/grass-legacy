CC = cc

CFLAGS = -g -I../

#------------- end of editables ----------------------------

T =

L = pj_lib.a

# projections
PLIB = \
	$L(PJ_aea.o) $L(PJ_aeqd.o) $L(PJ_airy.o) $L(PJ_aitoff.o) \
	$L(PJ_august.o) $L(PJ_bacon.o) $L(PJ_bipc.o) $L(PJ_boggs.o) \
	$L(PJ_bonne.o) $L(PJ_cass.o) $L(PJ_cc.o) $L(PJ_cea.o) \
	$L(PJ_collg.o) $L(PJ_dense.o) $L(PJ_eck1.o) $L(PJ_eck2.o) \
	$L(PJ_eck3.o) $L(PJ_eck4.o) $L(PJ_eck5.o) $L(PJ_eck6.o) \
	$L(PJ_eisen.o) $L(PJ_eqc.o) $L(PJ_eqdc.o) $L(PJ_fourn.o) \
	$L(PJ_gall.o) $L(PJ_gnom.o) $L(PJ_hammer.o) $L(PJ_hataea.o) \
	$L(PJ_laea.o) $L(PJ_lagrng.o) $L(PJ_lcc.o) $L(PJ_loxim.o) \
	$L(PJ_mbtfpp.o) $L(PJ_mbtfpq.o) $L(PJ_mbtfps.o) $L(PJ_merc.o) \
	$L(PJ_mill.o) $L(PJ_moll.o) $L(PJ_nocol.o) $L(PJ_nsper.o) \
	$L(PJ_ocea.o) $L(PJ_omerc.o) $L(PJ_ortho.o) $L(PJ_parab.o) \
	$L(PJ_pconic.o) $L(PJ_poly.o) $L(PJ_putp2.o) $L(PJ_putp5.o) \
	$L(PJ_quau.o) $L(PJ_robin.o) $L(PJ_rpoly.o) $L(PJ_sinu.o) \
	$L(PJ_stere.o) $L(PJ_tcc.o) $L(PJ_tcea.o) $L(PJ_tmerc.o) \
	$L(PJ_vandg.o) $L(PJ_vandg2.o) $L(PJ_vandg4.o) $L(PJ_wag7.o) \
	$L(PJ_wink1.o) $L(PJ_goode.o) $L(PJ_chamb.o) $L(PJ_lsat.o) \
	$L(PJ_mod_ster.o) $L(PJ_tpeqd.o)

# projection and access support
SLIB = \
	$L(pj_auth.o) $L(pj_fwd.o) $L(pj_init.o) $L(pj_inv.o) \
	$L(pj_msfn.o) $L(pj_mlfn.o) $L(pj_phi2.o) $L(pj_qsfn.o) \
	$L(pj_tsfn.o) $L(gen_cheby.o) $L(bchgen.o) $L(pj_list.o) \
	$L(pj_ellps.o) $L(pj_factors.o) $L(pj_deriv.o) $L(aasincos.o) \
	$L(emess.o)

# general procedures
MLIB = \
	$L(adjlon.o) $L(dmstor.o) $L(rtodms.o)

proj:	proj.o $T $L
	$(CC) -o proj proj.o $T $L -lm

libpj:	$(SLIB) $(MLIB) $(PLIB)
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ] ; then ranlib $L ; fi

install: libpj proj

clean:
	/bin/rm -f core *.o

$(SLIB) $(MLIB) $(PLIB): projects.h

$(SLIB): emess.h

emess.h:	emess.c proj.c

