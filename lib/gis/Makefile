MODULE_TOPDIR = ../..

EXTRA_CFLAGS=$(GETHOSTNAME) $(ZLIBINCPATH)

LIB_NAME = $(GIS_LIBNAME)

LIB_OBJS = \
	adj_cellhd.o\
	align_window.o\
	alloc.o\
	alloc_cell.o\
	area.o\
	area_ellipse.o\
	area_sphere.o\
	area_poly1.o\
	area_poly2.o\
	ascii_chk.o\
	ask.o\
	ask_cell.o\
	ask_vctrs.o\
	asprintf.o\
	auto_mask.o\
	bres_line.o\
	cats.o\
	cell_stats.o\
	cellstats_eq.o\
	cell_title.o\
	chop.o\
	clear_scrn.o\
	clicker.o\
	closecell.o\
	color_asp.o \
	color_byg.o \
	color_byr.o \
	color_free.o \
	color_get.o \
	color_grey.o \
	color_gyr.o \
	color_hist.o \
	color_init.o \
	color_insrt.o \
	color_invrt.o \
	color_look.o \
	color_org.o \
	color_rain.o \
	color_ramp.o \
	color_rand.o \
	color_range.o \
	color_read.o \
	color_rule.o \
	color_ryg.o \
	color_set.o \
	color_str.o \
	color_shift.o \
	color_wave.o \
	color_write.o \
	commas.o\
	copy.o\
	dalloc.o\
	date.o\
	datum.o\
	debug.o\
	distance.o\
	done_msg.o\
	eigen_tools.o\
	endian.o\
	env.o\
	error.o\
	file_name.o\
	find_cell.o\
	find_file.o\
	find_vect.o\
	flate.o\
	fork.o\
	format.o\
	fpreclass.o\
	geo_init.o\
	geodesic.o\
	geodist.o\
	get_cellhd.o\
	get_datum.o\
	get_datum_name.o\
	get_ellipse.o\
	get_ell_name.o\
	get_projname.o\
	get_projinfo.o\
	get_row.o\
	get_row_colr.o\
	get_window.o\
	getl.o\
	gets.o\
	gisbase.o\
	gisdbase.o\
	gishelp.o\
	gisinit.o\
	histo_eq.o\
	histogram.o\
	history.o\
	home.o\
	ialloc.o\
	icon.o\
	index.o\
	init_map.o\
	intersect.o\
	intr_char.o\
	key_value1.o\
	key_value2.o\
	key_value3.o\
	key_value4.o\
	legal_name.o\
	list.o\
	line_dist.o\
	ll_format.o\
	ll_scan.o\
	location.o\
	lu.o\
	mach_name.o\
	make_colr.o\
	mapcase.o\
	mapset.o\
	mapset_msc.o\
	mapset_nme.o\
	mask_info.o\
	maskfd.o\
	myname.o\
	named_colr.o\
	nme_in_mps.o\
	nl_to_spaces.o\
	null_val.o\
	open.o\
	opencell.o\
	parser.o\
	percent.o\
	plot.o\
	popen.o\
	pole_in_poly.o\
	progrm_nme.o\
	proj1.o\
	proj2.o\
	proj3.o\
	put_cellhd.o\
	put_row.o\
	put_title.o\
	put_window.o\
	putenv.o\
	quant.o\
	quant_io.o\
	quant_rw.o\
	radii.o\
	range.o\
	raster.o\
	rd_cellhd.o\
	readsites.o\
	readsites_xyz.o\
	reclass.o\
	remove.o\
	rename.o\
	rhumbline.o\
	rm_colr.o\
	set_prior.o\
	set_window.o\
	short_way.o\
	sites.o\
	sleep.o\
	squeeze.o\
	store.o\
	strings.o\
	strip.o\
	svd.o\
	system.o\
	tempfile.o\
	timestamp.o\
	token.o\
	wind_2_box.o\
	trim_dec.o\
	unctrl.o\
	unix_socks.o\
	view.o\
	whoami.o\
	wind_format.o\
	wind_limits.o\
	wind_overlap.o\
	wind_scan.o\
	window_map.o\
	wr_cellhd.o\
	writ_zeros.o\
	yes.o\
	zero.o\
	zero_cell.o\
	zone.o\
	G_dump.o

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: $(ARCH_INCDIR)/version.h \
	lib \
	$(ETC)/ellipse.table \
	$(ETC)/datum.table \
	$(ETC)/datumtransform.table \
	$(ETC)/projections \
	$(ETC)/FIPS.code \
	$(ETC)/state27 \
	$(ETC)/state83

$(ARCH_INCDIR)/version.h:
	echo 'static char *VERSION_STRING = "@(#) GRASS $(VERSION_NUMBER) ($(VERSION_DATE))";' > $@

$(ETC)/ellipse.table: ellipse.table
	rm -f $@
	cp $? $@
	chmod +r $@

$(ETC)/datum.table: datum.table
	rm -f $@
	cp $? $@
	chmod +r $@

$(ETC)/datumtransform.table: datumtransform.table
	rm -f $@
	cp $? $@
	chmod +r $@

$(ETC)/FIPS.code: FIPS.code
	rm -f $@
	cp $? $@
	chmod +r $@

$(ETC)/state27: state27
	rm -f $@
	cp $? $@
	chmod +r $@

$(ETC)/state83: state83
	rm -f $@
	cp $? $@
	chmod +r $@

$(ETC)/projections: projections
	rm -f $@
	cp $? $@
	chmod +r $@


$(OBJDIR)/area_ellipse.o: pi.h
$(OBJDIR)/area_sphere.o: pi.h
$(OBJDIR)/auto_mask.o: G.h
$(OBJDIR)/closecell.o: G.h
$(OBJDIR)/format.o: G.h
$(OBJDIR)/geodesic.o: pi.h
$(OBJDIR)/geodist.o: pi.h
$(OBJDIR)/get_row.o: G.h
$(OBJDIR)/get_window.o: G.h
$(OBJDIR)/gisinit.o: G.h
$(OBJDIR)/gisinit.o: $(ARCH_INCDIR)/version.h
$(OBJDIR)/maskfd.o: G.h
$(OBJDIR)/opencell.o: G.h
# openvect.o ***DEPRECATED*** delete next line for 5.1
$(OBJDIR)/openvect.o: G.h
$(OBJDIR)/put_row.o: G.h
$(OBJDIR)/radii.o: pi.h
$(OBJDIR)/rhumbline.o: pi.h
$(OBJDIR)/set_window.o: G.h
$(OBJDIR)/window_map.o: G.h

