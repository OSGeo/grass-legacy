MODULE_TOPDIR = ../..

LIB_NAME = $(GPROJ_LIBNAME)

EXTRA_CFLAGS = -I.

azimuthal = \
	PJ_aeqd.o PJ_gnom.o PJ_laea.o PJ_mod_ster.o \
	PJ_nsper.o PJ_nzmg.o PJ_ortho.o PJ_stere.o

conic = \
	PJ_aea.o PJ_bipc.o PJ_bonne.o PJ_eqdc.o \
	PJ_imw_p.o PJ_lcc.o PJ_mpoly.o PJ_poly.o \
	PJ_rpoly.o PJ_sconics.o PJ_krovak.o PJ_krovakgis.o

cylinder = \
	PJ_cass.o PJ_cc.o PJ_cea.o PJ_eqc.o \
	PJ_gall.o PJ_labrd.o PJ_lsat.o PJ_merc.o \
	PJ_mill.o PJ_ocea.o PJ_omerc.o PJ_somerc.o \
	PJ_tcc.o PJ_tcea.o PJ_tmerc.o

misc = \
	PJ_airy.o PJ_aitoff.o PJ_august.o PJ_bacon.o \
	PJ_chamb.o PJ_hammer.o PJ_lagrng.o PJ_larr.o \
	PJ_lask.o PJ_nocol.o PJ_ob_tran.o PJ_oea.o \
	PJ_tpeqd.o PJ_vandg.o PJ_vandg2.o PJ_vandg4.o \
	PJ_wag7.o

pseudo = \
	PJ_boggs.o PJ_collg.o PJ_crast.o PJ_denoy.o \
	PJ_eck1.o PJ_eck2.o PJ_eck3.o PJ_eck4.o \
	PJ_eck5.o PJ_fahey.o PJ_fouc_s.o PJ_gins8.o \
	PJ_gn_sinu.o PJ_goode.o PJ_hatano.o PJ_loxim.o \
	PJ_mbt_fps.o PJ_mbtfpp.o PJ_mbtfpq.o PJ_moll.o \
	PJ_nell.o PJ_nell_h.o PJ_putp2.o PJ_putp3.o \
	PJ_putp4p.o PJ_putp5.o PJ_putp6.o PJ_robin.o \
	PJ_sts.o PJ_urm5.o PJ_urmfps.o PJ_wag2.o \
	PJ_wag3.o PJ_wink1.o PJ_wink2.o

support = \
	aasincos.o adjlon.o bch2bps.o bchgen.o biveval.o \
	dmstor.o emess.o gen_cheb.o geocent.o geod.o geod_for.o \
	geod_inv.o geod_set.o mk_cheby.o nad_cvt.o nad_init.o \
	nad_intr.o p_series.o pj_apply_gridshift.o pj_auth.o \
	pj_datum_set.o pj_datums.o pj_deriv.o pj_ell_set.o pj_ellps.o \
	pj_errno.o pj_factors.o pj_fwd.o pj_init.o pj_inv.o \
	pj_latlong.o pj_list.o pj_malloc.o pj_mlfn.o pj_msfn.o \
	pj_open_lib.o pj_param.o pj_phi2.o pj_pr_list.o pj_qsfn.o \
	pj_release.o pj_strerrno.o pj_transform.o pj_tsfn.o pj_units.o \
	pj_utils.o pj_zpoly1.o rtodms.o strtod.o vector1.o

GRASSINT = get_proj.o do_proj.o do_proj_nad.o datum_shift.o 

LIB_OBJS = $(azimuthal) $(conic) $(cylinder) $(misc) $(pseudo) $(support) $(GRASSINT)

RLIB = pj_release.o

NAD_DIR = $(GISBASE)/etc/nad
NAD2BIN = $(GISBASE)/etc/nad2bin

NAD_TABLES = \
	$(NAD_DIR)/FL		\
	$(NAD_DIR)/MD		\
	$(NAD_DIR)/TN		\
	$(NAD_DIR)/WI		\
	$(NAD_DIR)/WO		\
	$(NAD_DIR)/alaska	\
	$(NAD_DIR)/conus	\
	$(NAD_DIR)/hawaii	\
	$(NAD_DIR)/prvi		\
	$(NAD_DIR)/stgeorge	\
	$(NAD_DIR)/stlrnc	\
	$(NAD_DIR)/stpaul	\
	$(NAD_DIR)/ntv1_can.dat

include $(MODULE_TOPDIR)/include/Make/Lib.make 

default: lib $(NAD_TABLES) 

$(NAD2BIN): nad2bin.o
	$(CC) $(LDFLAGS) $? -o $@

$(NAD_DIR):
	mkdir $(NAD_DIR)

$(NAD_DIR)/FL:		$(NAD_DIR) $(NAD2BIN) FL.lla
	$(NAD2BIN) < FL.lla $@

$(NAD_DIR)/MD:		$(NAD_DIR) $(NAD2BIN) MD.lla
	$(NAD2BIN) < MD.lla $@

$(NAD_DIR)/TN:		$(NAD_DIR) $(NAD2BIN) TN.lla
	$(NAD2BIN) < TN.lla $@

$(NAD_DIR)/WI:		$(NAD_DIR) $(NAD2BIN) WI.lla
	$(NAD2BIN) < WI.lla $@

$(NAD_DIR)/WO:		$(NAD_DIR) $(NAD2BIN) WO.lla
	$(NAD2BIN) < WO.lla $@

$(NAD_DIR)/alaska:	$(NAD_DIR) $(NAD2BIN) alaska.lla
	$(NAD2BIN) < alaska.lla $@

$(NAD_DIR)/conus:	$(NAD_DIR) $(NAD2BIN) conus.lla
	$(NAD2BIN) < conus.lla $@

$(NAD_DIR)/hawaii:	$(NAD_DIR) $(NAD2BIN) hawaii.lla
	$(NAD2BIN) < hawaii.lla $@

$(NAD_DIR)/prvi:	$(NAD_DIR) $(NAD2BIN) prvi.lla
	$(NAD2BIN) < prvi.lla $@

$(NAD_DIR)/stgeorge:	$(NAD_DIR) $(NAD2BIN) stgeorge.lla
	$(NAD2BIN) < stgeorge.lla $@

$(NAD_DIR)/stlrnc:	$(NAD_DIR) $(NAD2BIN) stlrnc.lla
	$(NAD2BIN) < stlrnc.lla $@

$(NAD_DIR)/stpaul:	$(NAD_DIR) $(NAD2BIN) stpaul.lla
	$(NAD2BIN) < stpaul.lla $@

$(NAD_DIR)/ntv1_can.dat:	$(NAD_DIR) ntv1_can.dat
	cp -f ntv1_can.dat $@

