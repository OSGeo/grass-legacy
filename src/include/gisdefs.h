/*
* $Id$
*
*****************************************************************************
*
* MODULE:   	Grass Include Files
* AUTHOR(S):	Original author unknown - probably CERL
*   	    	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
* PURPOSE:  	This file contains the prototypes for all the functions in the
*   	    	gis library (src/libes/gis).
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#ifndef GRASS_GISDEFS_H
#define GRASS_GISDEFS_H

/*============================= Include Files ==============================*/

/* none */

/*=========================== Constants/Defines ============================*/

/* none */

/*=========================== Typedefs/Structures ==========================*/

/* none */

/*============================== Prototypes ================================*/

/* adj_cellhd.c */
char *G_adjust_Cell_head(struct Cell_head *, int, int);

/* align_window.c */
char *G_align_window(struct Cell_head *, struct Cell_head *);

/* alloc.c */
char *G_malloc(int);
char *G_calloc(int, int);
char *G_realloc(void *, int);
int G_free(void *);

/* alloc_cell.c */
int G_raster_size(RASTER_MAP_TYPE);
CELL *G_allocate_cell_buf(void);
void *G_allocate_raster_buf(RASTER_MAP_TYPE);
CELL *G_allocate_c_raster_buf(void);
FCELL *G_allocate_f_raster_buf(void);
DCELL *G_allocate_d_raster_buf(void);
char *G_allocate_null_buf(void);
unsigned char *G__allocate_null_bits(int);
int G__null_bitstream_size(int);

/* area.c */
int G_begin_cell_area_calculations(void);
double G_area_of_cell_at_row(register int);
int G_begin_polygon_area_calculations(void);
double G_area_of_polygon(double *, double *, int);

/* area_ellipse.c */
int G_begin_zone_area_on_ellipsoid(double, double, double);
double G_darea0_on_ellipsoid(register double);
double G_area_for_zone_on_ellipsoid(register double, register double);

/* area_poly1.c */
int G_begin_ellipsoid_polygon_area(double, double);
double G_ellipsoid_polygon_area(double *, double *, int);

/* area_poly2.c */
double G_planimetric_polygon_area(double *, double *, int);

/* area_sphere.c */
int G_begin_zone_area_on_sphere(double, double);
double G_darea0_on_sphere(register double);
double G_area_for_zone_on_sphere(register double, register double);

/* ascii_chk.c */
int G_ascii_check(char *);

/* ask.c */
char *G_ask_new(char *, char *, char *, char *);
char *G_ask_new_ext(char *, char *, char *, char *, char *, int (*)());
char *G_ask_old(char *, char *, char *, char *);
char *G_ask_old_ext(char *, char *, char *, char *, char *, int (*)());
char *G_ask_any(char *, char *, char *, char *, int);
char *G_ask_any_ext(char *, char *, char *, char *, int, char *, int (*)());
char *G_ask_in_mapset(char *, char *, char *, char *);
char *G_ask_in_mapset_ext(char *, char *, char *, char *, char *, int (*)());
int G_set_ask_return_msg(char *);
char *G_get_ask_return_msg(void);

/* ask_cell.c */
char *G_ask_cell_new(char *, char *);
char *G_ask_cell_old(char *, char *);
char *G_ask_cell_in_mapset(char *, char *);
char *G_ask_cell_any(char *, char *);

/* ask_vctrs.c */
char *G_ask_vector_new(char *, char *);
char *G_ask_vector_old(char *, char *);
char *G_ask_vector_any(char *, char *);
char *G_ask_vector_in_mapset(char *, char *);

/* auto_mask.c */
int G__check_for_auto_masking(void);
int G_suppress_masking(void);
int G_unsuppress_masking(void);

/* bres_line.c */
int G_bresenham_line(register int, register int, int, int, int (*)());

/* cats.c */
int G_read_cats(char *, char *, struct Categories *);
int G_read_raster_cats(char *, char *, struct Categories *);
int G_read_vector_cats(char *, char *, struct Categories *);
CELL G_number_of_cats(char *, char *);
CELL G__read_cats(char *, char *, char *, struct Categories *, int);
char *G_get_cats_title(struct Categories *);
char *G_get_raster_cats_title(struct Categories *);
char *G_get_cat(CELL, struct Categories *);
char *G_get_c_raster_cat(CELL *, struct Categories *);
char *G_get_f_raster_cat(FCELL *, struct Categories *);
char *G_get_d_raster_cat(DCELL *, struct Categories *);
char *G_get_raster_cat(void *, struct Categories *, RASTER_MAP_TYPE);
int G_unmark_raster_cats(struct Categories *);
int G_mark_c_raster_cats(CELL *, int, struct Categories *);
int G_mark_f_raster_cats(FCELL *, int, struct Categories *);
int G_mark_d_raster_cats(DCELL *, int, struct Categories *);
int G_mark_raster_cats(void *, int, struct Categories *, RASTER_MAP_TYPE);
int G_rewind_raster_cats(struct Categories *);
char *G_get_next_marked_d_raster_cat(struct Categories *, DCELL *, DCELL *,
    long *);
char *G_get_next_marked_c_raster_cat(struct Categories *, CELL *, CELL *,
    long *);
char *G_get_next_marked_f_raster_cat(struct Categories *, FCELL *, FCELL *,
    long *);
char *G_get_next_marked_raster_cat(struct Categories *, void *, void *, long *,
    RASTER_MAP_TYPE);
int G_set_cat(CELL, char *, struct Categories *);
int G_set_c_raster_cat(CELL *, CELL *, char *, struct Categories *);
int G_set_f_raster_cat(FCELL *, FCELL *, char *, struct Categories *);
int G_set_d_raster_cat(DCELL *, DCELL *, char *, struct Categories *);
int G_set_raster_cat(void *, void *, char *, struct Categories *,
    RASTER_MAP_TYPE);
int G_write_cats(char *, struct Categories *);
int G_write_raster_cats(char *, struct Categories *);
int G_write_vector_cats(char *, struct Categories *);
int G__write_cats(char *, char *, struct Categories *);
char *G_get_ith_d_raster_cat(struct Categories *, int, DCELL *, DCELL *);
char *G_get_ith_f_raster_cat(struct Categories *, int, void *, void *);
char *G_get_ith_c_raster_cat(struct Categories *, int, void *, void *);
char *G_get_ith_raster_cat(struct Categories *, int, void *, void *,
    RASTER_MAP_TYPE);
int G_init_cats(CELL, char *, struct Categories *);
int G_init_raster_cats(char *, struct Categories *);
int G_set_cats_title(char *, struct Categories *);
int G_set_raster_cats_title(char *, struct Categories *);
int G_set_cats_fmt(char *, double, double, double, double, struct Categories *);
int G_set_raster_cats_fmt(char *, double, double, double, double,
    struct Categories *);
int G_free_cats(struct Categories *);
int G_free_raster_cats(struct Categories *);
int G_copy_raster_cats(struct Categories *, struct Categories *);
int G_number_of_raster_cats(struct Categories *);
int G_sort_cats(struct Categories *);

/* cell_stats.c */
int G_init_cell_stats(struct Cell_stats *);
int G_update_cell_stats(CELL *, int, struct Cell_stats *);
int G_find_cell_stat(CELL, long *, struct Cell_stats *);
int G_rewind_cell_stats(struct Cell_stats *);
int G_next_cell_stat(CELL *, long *, struct Cell_stats *);
int G_get_stats_for_null_value(long *, struct Cell_stats *);
int G_free_cell_stats(struct Cell_stats *);

/* cell_title.c */
char *G_get_cell_title(char *, char *);

/* cellstats_eq.c */
int G_cell_stats_histo_eq(struct Cell_stats *, CELL, CELL, CELL, CELL, int,
    void (*)());

/* clear_scrn.c */
int G_clear_screen(void);

/* clicker.c */
int G_clicker (void);

/* closecell.c */
int G_close_cell(int);
int G_unopen_cell(int);
int G__write_fp_format(int);

/* color_asp.c */
int G_make_aspect_colors(struct Colors *, CELL, CELL);
int G_make_aspect_fp_colors(struct Colors *, DCELL, DCELL);
int G_add_aspect_colors(struct Colors *, CELL, CELL);

/* color_free.c */
int G_free_colors(struct Colors *);
int G__color_free_rules(struct _Color_Info_ *);
int G__color_free_lookup(struct _Color_Info_ *);
int G__color_free_fp_lookup(struct _Color_Info_ *);
int G__color_reset(struct Colors *);

/* color_get.c */
int G_get_color(CELL, int *, int *, int *, struct Colors *);
int G_get_raster_color(void *, int *, int *, int *, struct Colors *,
    RASTER_MAP_TYPE);
int G_get_c_raster_color(CELL *, int *, int *, int *, struct Colors *);
int G_get_f_raster_color(FCELL *, int *, int *, int *, struct Colors *);
int G_get_d_raster_color(DCELL *, int *, int *, int *, struct Colors *);
int G_get_null_value_color(int *, int *, int *, struct Colors *);
int G_get_default_color(int *, int *, int *, struct Colors *);

/* color_grey.c */
int G_make_grey_scale_colors(struct Colors *, CELL, CELL);
int G_make_grey_scale_fp_colors(struct Colors *, double, double);
int G_make_grey_scale(struct Colors *, CELL, CELL);
int G_add_grey_scale_colors(struct Colors *, CELL, CELL);

/* color_gyr.c */
int G_make_gyr_colors(struct Colors *, CELL, CELL);
int G_make_gyr_fp_colors(struct Colors *, DCELL, DCELL);
int G_add_gyr_colors(struct Colors *, CELL, CELL);

/* color_hist.c */
int G_make_histogram_eq_colors(struct Colors *, struct Cell_stats *);

/* color_init.c */
int G_init_colors(struct Colors *);

/* color_insrt.c */
int G__insert_color_into_lookup(CELL, int, int, int, struct _Color_Info_ *);

/* color_invrt.c */
int G_invert_colors(struct Colors *);

/* color_look.c */
int G_lookup_colors(CELL *, unsigned char *, unsigned char *, unsigned char *,
    unsigned char *, int, struct Colors *);
int G_lookup_c_raster_colors(CELL *, unsigned char *, unsigned char *,
    unsigned char *, unsigned char *, int, struct Colors *);
int G_lookup_raster_colors(void *, unsigned char *, unsigned char *,
    unsigned char *, unsigned char *, int, struct Colors *, RASTER_MAP_TYPE);
int G_lookup_f_raster_colors(DCELL *, unsigned char *, unsigned char *,
    unsigned char *, unsigned char *, int, struct Colors *);
int G_lookup_d_raster_colors(DCELL *, unsigned char *, unsigned char *,
    unsigned char *, unsigned char *, int, struct Colors *);
int G__lookup_colors(void *, unsigned char *, unsigned char *, unsigned char *,
    unsigned char *, int, struct Colors *, int, int, RASTER_MAP_TYPE);
int G__interpolate_color_rule(DCELL, unsigned char *, unsigned char *,
    unsigned char *, struct _Color_Rule_ *);

/* color_org.c */
int G__organize_colors(struct Colors *);

/* color_rain.c */
int G_make_rainbow_colors(struct Colors *, CELL, CELL);
int G_make_rainbow_fp_colors(struct Colors *, DCELL, DCELL);
int G_add_rainbow_colors(struct Colors *, CELL, CELL);

/* color_ramp.c */
int G_make_ramp_colors(struct Colors *, CELL, CELL);
int G_make_ramp_fp_colors(struct Colors *, DCELL, DCELL);
int G_make_color_ramp(struct Colors *, CELL, CELL);
int G_add_ramp_colors(struct Colors *, CELL, CELL);

/* color_rand.c */
int G_make_random_colors(struct Colors *, CELL, CELL);

/* color_range.c */
int G_set_color_range(CELL, CELL, struct Colors *);
int G_set_d_color_range(DCELL, DCELL, struct Colors *);
int G_get_color_range(CELL *, CELL *, struct Colors *);
int G_get_d_color_range(DCELL *, DCELL *, struct Colors *);

/* color_read.c */
int G_read_colors(char *, char *, struct Colors *);
int G_mark_colors_as_fp(struct Colors *);

/* color_rule.c */
int G_add_d_raster_color_rule(DCELL *, int, int, int, DCELL *, int, int, int,
    struct Colors *);
int G_add_f_raster_color_rule(FCELL *, int, int, int, FCELL *, int, int, int,
    struct Colors *);
int G_add_c_raster_color_rule(CELL *, int, int, int, CELL *, int, int, int,
    struct Colors *);
int G_add_raster_color_rule(void *, int, int, int, void *, int, int, int,
    struct Colors *, RASTER_MAP_TYPE);
int G_add_color_rule(CELL, int, int, int, CELL, int, int, int, struct Colors *);
int G_add_modular_d_raster_color_rule(DCELL *, int, int, int, DCELL *, int,
    int, int, struct Colors *);
int G_add_modular_f_raster_color_rule(FCELL *, int, int, int, FCELL *, int,
    int, int, struct Colors *);
int G_add_modular_c_raster_color_rule(CELL *, int, int, int, CELL *, int, int,
    int, struct Colors *);
int G_add_modular_raster_color_rule(void *, int, int, int, void *, int, int,
    int, struct Colors *, RASTER_MAP_TYPE);
int G_add_modular_color_rule(CELL, int, int, int, CELL, int, int, int,
    struct Colors *);

/* color_ryg.c */
int G_make_ryg_colors(struct Colors *, CELL, CELL);
int G_make_ryg_fp_colors(struct Colors *, DCELL, DCELL);
int G_make_red_yel_grn(struct Colors *, CELL, CELL);
int G_add_ryg_colors(struct Colors *, CELL, CELL);

/* color_set.c */
int G_set_color(CELL, int, int, int, struct Colors *);
int G_set_d_color(DCELL, int, int, int, struct Colors *);
int G_set_null_value_color(int, int, int, struct Colors *);
int G_set_default_color(int, int, int, struct Colors *);

/* color_shift.c */
int G_shift_colors(int, struct Colors *);
int G_shift_d_colors(DCELL, struct Colors *);

/* color_wave.c */
int G_make_wave_colors(struct Colors *, CELL, CELL);
int G_make_wave_fp_colors(struct Colors *, DCELL, DCELL);
int G_make_color_wave(struct Colors *, CELL, CELL);
int G_add_wave_colors(struct Colors *, CELL, CELL);

/* color_write.c */
int G_write_colors(char *, char *, struct Colors *);
int G__write_colors(FILE *, struct Colors *);

/* commas.c */
int G_insert_commas(char *);
int G_remove_commas(char *);

/* copy.c */
int G_copy(void *, void *, int);

/* dalloc.c */
double *G_alloc_vector(int);
double **G_alloc_matrix(int, int);
float *G_alloc_fvector(int);
float **G_alloc_fmatrix(int, int);
int G_free_vector(double *);
int G_free_matrix(double **);
int G_free_fmatrix(float **);

/* date.c */
char *G_date(void);

/* def_window.c */
int G_def_window(struct Cell_head *);

/* distance.c */
int G_begin_distance_calculations(void);
double G_distance(double, double, double, double);
double G_distance_between_line_segments(double, double, double, double, double,
    double, double, double);
double G_distance_point_to_line_segment(double, double, double, double, double,
    double);

/* done_msg.c */
int G_done_msg(char *);

/* edit_cats.c */
int G_edit_cats(char *, struct Categories *, int);
int G_edit_fp_cats(char *, struct Categories *);

/* edit_cellhd.c */
int G_edit_cellhd(struct Cell_head *, int);

/* edit_hist.c */
int G_edit_history(struct History *);

/* eigen_tools.c */
int G_tqli(double [], double [], int, double **);
void G_tred2(double **, int, double [], double []);

/* env.c */
char *G_getenv(char *);
char *G__getenv(char *);
int G_setenv(char *, char *);
int G__setenv(char *, char *);
int G_unsetenv(char *);
int G__write_env(void);
char *G__env_name(int);
int G__read_env(void);
int G__set_gisrc_file(char *);
char *G__get_gisrc_file(void);
int G__create_alt_env(void);
int G__switch_env(void);

/* error.c */
#ifdef __GNUC_MINOR__
int G_fatal_error(char *,...) __attribute__ ((__noreturn__));
#else
int G_fatal_error(char *,...);
#endif
int G_warning(char *,...);
int G_suppress_warnings(int);
int G_sleep_on_error(int);
int G_set_error_routine(int (*)());
int G_unset_error_routine(void);

/* file_name.c */
char *G__file_name(char *, char *, char *, char *);

/* find_cell.c */
char *G_find_cell(char *, char *);
char *G_find_cell2(char *, char *);

/* find_file.c */
char *G_find_file(char *, char *, char *);
char *G_find_file2(char *, char *, char *);

/* find_vect.c */
char *G_find_vector(char *, char *);
char *G_find_vector2(char *, char *);

/* flate.c */
int G_zlib_compress(unsigned char *, int, unsigned char *, int);
int G_zlib_expand(unsigned char *, int, unsigned char *, int);
int G_zlib_write(int, unsigned char *, int);
int G_zlib_read(int, int, unsigned char *, int);
int G_zlib_write_noCompress (int, unsigned char *, int);

/* fork.c */
int G_fork(void);

/* format.c */
int G__check_format(int);
int G__read_row_ptrs(int);
int G__write_row_ptrs(int);

/* fpreclass.c */
void G_fpreclass_clear(struct FPReclass *);
void G_fpreclass_reset(struct FPReclass *);
int G_fpreclass_init(struct FPReclass *);
void G_fpreclass_set_domain(struct FPReclass *, DCELL, DCELL);
void G_fpreclass_set_range(struct FPReclass *, DCELL, DCELL);
int G_fpreclass_get_limits(struct FPReclass *, DCELL *, DCELL *, DCELL *,
    DCELL *);
int G_fpreclass_nof_rules(struct FPReclass *);
void G_fpreclass_get_ith_rule(struct FPReclass *, int, DCELL *, DCELL *,
    DCELL *, DCELL *);
void G_fpreclass_set_neg_infinite_rule(struct FPReclass *, DCELL, DCELL);
int G_fpreclass_get_neg_infinite_rule(struct FPReclass *, DCELL *, DCELL *);
void G_fpreclass_set_pos_infinite_rule(struct FPReclass *, DCELL, DCELL);
int G_fpreclass_get_pos_infinite_rule(struct FPReclass *, DCELL *, DCELL *);
void G_fpreclass_add_rule(struct FPReclass *, DCELL, DCELL, DCELL, DCELL);
void G_fpreclass_reverse_rule_order(struct FPReclass *);
DCELL G_fpreclass_get_cell_value(struct FPReclass *, DCELL);
void G_fpreclass_perform_di(struct FPReclass *, DCELL *, CELL *, int);
void G_fpreclass_perform_df(struct FPReclass *, DCELL *, FCELL *, int);
void G_fpreclass_perform_dd(struct FPReclass *, DCELL *, DCELL *, int);
void G_fpreclass_perform_fi(struct FPReclass *, FCELL *, CELL *, int);
void G_fpreclass_perform_ff(struct FPReclass *, FCELL *, FCELL *, int);
void G_fpreclass_perform_fd(struct FPReclass *, FCELL *, DCELL *, int);
void G_fpreclass_perform_ii(struct FPReclass *, CELL *, CELL *, int);
void G_fpreclass_perform_if(struct FPReclass *, CELL *, FCELL *, int);
void G_fpreclass_perform_id(struct FPReclass *, CELL *, DCELL *, int);

/* geodesic.c */
int G_begin_geodesic_equation(double, double, double, double);
double G_geodesic_lat_from_lon(double);

/* geodist.c */
int G_begin_geodesic_distance(double, double);
int G_set_geodesic_distance_lat1(double);
int G_set_geodesic_distance_lat2(double);
double G_geodesic_distance_lon_to_lon(double, double);
double G_geodesic_distance(double, double, double, double);

/* get_cellhd.c */
int G_get_cellhd(char *, char *, struct Cell_head *);

/*get_datum.c */
int G_get_datum_parameters(double *, double *, double *, double *, double *,
    double *);
int G_get_datum_parameters7(double *, double *, double *, double *, double *,
    double *, double *, double *, double *, double *);

/* get_datum_name.c */
int G_ask_datum_name(char *);

/* get_ell_name.c */
int G_ask_ellipse_name(char *);

/* get_ellipse.c */
int G_get_ellipsoid_parameters(double *, double *);
int G_get_spheroid_by_name(const char *, double *, double *, double*);
int G_get_ellipsoid_by_name(const char *, double *, double *);
char *G_ellipsoid_name(int);
char *G_ellipsoid_description(int);

/* get_projinfo.c */
struct Key_Value *G_get_projunits(void);
struct Key_Value *G_get_projinfo(void);

/* get_projname.c */
int G_ask_proj_name(char *, char *);

/* get_row.c */
int G_get_map_row_nomask(int, CELL *, int);
int G_get_raster_row_nomask(int, void *, int, RASTER_MAP_TYPE);
int G_get_c_raster_row_nomask(int, CELL *, int);
int G_get_f_raster_row_nomask(int, FCELL *, int);
int G_get_d_raster_row_nomask(int, DCELL *, int);
int G_get_map_row(int, CELL *, int);
int G_get_raster_row(int, void *, int, RASTER_MAP_TYPE);
int G_get_c_raster_row(int, CELL *, int);
int G_get_f_raster_row(int, FCELL *, int);
int G_get_d_raster_row(int, DCELL *, int);
int G_get_null_value_row(int, char *, int);
int G_get_null_value_row_nomask(int, char *, int);
int G__open_null_read(int);
int G__read_null_bits(int, unsigned char *, int, int, int);
int embed_nulls_nomask(int, void *, int, RASTER_MAP_TYPE, int);
int embed_nulls(int, void *, int, RASTER_MAP_TYPE, int);

/* get_window.c */
int G_get_window(struct Cell_head *);
int G_get_default_window(struct Cell_head *);
char *G__get_window(struct Cell_head *, char *, char *, char *);

/* getl.c */
int G_getl(char *, int, FILE *);

/* gets.c */
int G_gets(char *);

/* gisbase.c */
char *G_gisbase(void);

/* gisdbase.c */
char *G_gisdbase(void);

/* gishelp.c */
int G_gishelp(char *, char *);

/* gisinit.c */
int G_gisinit(char *);
int G_no_gisinit(void);
int G__check_gisinit(void);

/* grid_dist1.c */
double G_ellipsoid_grid_dist(double, double, double, double, double, double);

/* grid_dist2.c */
double G_ellipsoid_grid_dist(double, double, double, double, double, double);

/* histo_eq.c */
int G_histogram_eq(struct Histogram *, unsigned char **, CELL *, CELL *);

/* histogram.c */
int G_init_histogram(struct Histogram *);
int G_read_histogram(char *, char *, struct Histogram *);
int G_write_histogram(char *, struct Histogram *);
int G_write_histogram_cs(char *, struct Cell_stats *);
int G_make_histogram_cs(struct Cell_stats *, struct Histogram *);
int G_get_histogram_num(struct Histogram *);
CELL G_get_histogram_cat(int, struct Histogram *);
long G_get_histogram_count(int, struct Histogram *);
int G_free_histogram(struct Histogram *);
int G_sort_histogram(struct Histogram *);
int G_sort_histogram_by_count(struct Histogram *);
int G_remove_histogram(char *);
int G_add_histogram(CELL, long, struct Histogram *);
int G_set_histogram(CELL, long, struct Histogram *);
int G_extend_histogram(CELL, long, struct Histogram *);
int G_zero_histogram(struct Histogram *);

/* history.c */
int G_read_history(char *, char *, struct History *);
int G_write_history(char *, struct History *);
int G_short_history(char *, char *, struct History *);

/* home.c */
char *G_home(void);
char *G__home(void);

/* ialloc.c */
int *G_alloc_ivector(int);
int **G_alloc_imatrix(int, int);
int G_free_ivector(int *);
int G_free_imatrix(int **);

/* index.c */
char *G_index(register char *, int);
char *G_rindex(register char *, int);

/* init_map.c */
int G__random_d_initialize_0(int, int, int);
int G__random_f_initialize_0(int, int, int);

/* intersect.c */
int G_intersect_line_segments(double, double, double, double, double, double,
    double, double, double *, double *, double *, double *);

/* intr_char.c */
char G_intr_char(void);

/* key_value1.c */
struct Key_Value *G_create_key_value(void);
int G_set_key_value(char *, char *, struct Key_Value *);
char *G_find_key_value(char *, struct Key_Value *);
int G_free_key_value(struct Key_Value *);

/* key_value2.c */
int G_fwrite_key_value(FILE *, struct Key_Value *);
struct Key_Value *G_fread_key_value(FILE *);

/* key_value3.c */
int G_write_key_value_file(char *, struct Key_Value *, int *);
struct Key_Value *G_read_key_value_file(char *, int *);

/* key_value4.c */
int G_update_key_value_file(char *, char *, char *);
int G_lookup_key_value_from_file(char *, char *, char [], int);

/* legal_name.c */
int G_legal_filename(char *);

/* line_dist.c */
int G_set_distance_to_line_tolerance(double);
double G_distance2_point_to_line(double, double, double, double, double,
    double);

/* list.c */
int G_set_list_hit_return(int);
int G_list_element(char *, char *, char *, int (*)());

/* ll_format.c */
int G_lat_format(double, char *);
char *G_lat_format_string(void);
int G_lon_format(double, char *);
char *G_lon_format_string(void);
int G_llres_format(double, char *);
char *G_llres_format_string(void);
int G_lat_parts(double, int *, int *, double *, char *);
int G_lon_parts(double, int *, int *, double *, char *);

/* ll_scan.c */
int G_lat_scan(char *, double *);
int G_lon_scan(char *, double *);
int G_llres_scan(char *, double *);

/* location.c */
char *G_location_path(void);
char *G_location(void);
char *G__location_path(void);

/* lu.c */
int G_ludcmp(double **, int, int *, double *);
void G_lubksb(double **, int, int *, double []);

/* lzw.c */
unsigned char *lzw_decode(unsigned char *, unsigned int);
int lzw_expand(int (*)(), int (*)());
int G_lzw_nof_read_bytes(void);
int G_lzw_max_used_bits(void);
void G_lzw_set_bits(int);
int G_lzw_compress(unsigned char *, unsigned char *, int);
int G_lzw_expand(unsigned char *, unsigned char *, int);
int G_lzw_compress_count_only_array(unsigned char *, int);
int G_lzw_compress_count_only_file(int);
int G_lzw_write(int, unsigned char *, int);
int G_lzw_write_noCompress(int, unsigned char *, int);
int G_lzw_test_status(int);
int G_lzw_read2(int, unsigned char *, int, int);
int G_lzw_read(int, unsigned char *, int);
int G_lzw_transfer_compress(int, int, int);
int G_lzw_transfer_expand(int, int, int);

/* mach_name.c */
char *G__machine_name(void);

/* make_colr.c */
int G_make_colors(char *, char *, struct Colors *);

/* mapcase.c */
char *G_tolcase(char *);
char *G_toucase(char *);

/* mapset.c */
char *G_mapset(void);
char *G__mapset(void);

/* mapset_msc.c */
int G__make_mapset_element(char *);
int G__mapset_permissions(char *);

/* mapset_nme.c */
char *G__mapset_name(int);
int G__create_alt_search_path(void);
int G__switch_search_path(void);
int G_reset_mapsets(void);

/* mask_info.c */
char *G_mask_info(void);
int G__mask_info(char *, char *);

/* maskfd.c */
int G_maskfd(void);

/* myname.c */
char *G_myname(void);

/* named_colr.c */
int G_color_values(char *, float *, float *, float *);
char *G_color_name(int);

/* nl_to_spaces.c */
void G_newlines_to_spaces(char *);

/* nme_in_mps.c */
int G__name_in_mapset(char *, char *, char *);
int G__name_is_fully_qualified(char *, char *, char *);
char *G_fully_qualified_name(char *, char *);

/* null_val.c */
int G__check_null_bit(unsigned char *, int, int);
int G__set_flags_from_01_random(char *, unsigned char *, int, int, int);
int G__convert_01_flags(char *, unsigned char *, int);
int G__convert_flags_01(char *, unsigned char *, int);
int G__init_null_bits(unsigned char *, int);
int G__init_null_patterns(void);
int G__set_null_value(void *, int, int, RASTER_MAP_TYPE);
int G_set_null_value(void *, int, RASTER_MAP_TYPE);
int G_set_f_null_value(FCELL *, int);
int G_set_d_null_value(DCELL *, int);
int G_set_c_null_value(CELL *, int);
int G_is_f_null_value(FCELL *);
int G_is_d_null_value(DCELL *);
int G_is_c_null_value(CELL *);
int G_is_null_value(void *, RASTER_MAP_TYPE);
int G_insert_f_null_values(FCELL *, char *, int);
int G_insert_d_null_values(DCELL *, char *, int);
int G_insert_c_null_values(CELL *, char *, int);
int G_insert_null_values(void *, char *, int, RASTER_MAP_TYPE);

/* open.c */
int G__open(char *, char *, char *, int);
int G_open_new(char *, char *);
int G_open_old(char *, char *, char *);
int G_open_update(char *, char *);
FILE *G_fopen_new(char *, char *);
FILE *G_fopen_old(char *, char *, char *);
FILE *G_fopen_append(char *, char *);
FILE *G_fopen_modify(char *, char *);

/* opencell.c */
int G_open_cell_old(char *, char *);
int G__open_cell_old(char *, char *);
int G_open_cell_new(char *);
int G_open_cell_new_random(char *);
int G_open_cell_new_uncompressed(char *);
int G_want_histogram(int);
int G_set_cell_format(int);
int G_cellvalue_format(CELL);
int G_open_fp_cell_new(char *);
int G_open_fp_cell_new_uncompressed(char *);
int G__open_raster_new(char *, int);
int G__reallocate_work_buf(int);
int G__reallocate_null_buf(void);
int G__reallocate_mask_buf(void);
int G_set_fp_type(RASTER_MAP_TYPE);
int G_raster_map_is_fp(char *, char *);
RASTER_MAP_TYPE G_raster_map_type(char *, char *);
RASTER_MAP_TYPE G__check_fp_type(char *, char *);
int G_open_raster_new(char *, RASTER_MAP_TYPE);
int G_open_raster_new_uncompressed(char *, RASTER_MAP_TYPE);
int G_set_quant_rules(int, struct Quant *);

/* parser.c */
int G_disable_interactive(void);
struct GModule *G_define_module(void);
struct Flag *G_define_flag(void);
struct Option *G_define_option(void);
int G_parser(int, char **);
int G_usage(void);
char *G_recreate_command(void);

/* percent.c */
int G_percent(int, int, int);

/* plot.c */
int G_setup_plot(double, double, double, double, int (*)(), int (*)());
int G_plot_where_xy(double, double, int *, int *);
int G_plot_where_en(int, int, double *, double *);
int G_plot_point(double, double);
int G_plot_line(double, double, double, double);
int G_plot_line2(double, double, double, double);
int G_plot_polygon(double *, double *, int);
int G_plot_fx(double (*)(), double, double);

/* pole_in_poly.c */
int G_pole_in_polygon(double *, double *, int);

/* popen.c */
FILE *G_popen(char *, char *);
int G_pclose(FILE *);

/* progrm_nme.c */
char *G_program_name(void);
int G_set_program_name(char *);

/* proj1.c */
int G_projection(void);

/* proj2.c */
int G__projection_units(int);
char *G__unit_name(int, int);
char *G__projection_name(int);

/* proj3.c */
char *G_database_unit_name(int);
char *G_database_projection_name(void);
double G_database_units_to_meters_factor(void);
char *G_database_datum_name();
char *G_database_ellipse_name();

/* put_cellhd.c */
int G_put_cellhd(char *, struct Cell_head *);

/* put_row.c */
int G_zeros_r_nulls(int);
int G_put_map_row(int, CELL *);
int G_put_map_row_random(int, CELL *, int, int, int);
int G__put_null_value_row(int, char *);
int G_put_raster_row(int, void *, RASTER_MAP_TYPE);
int G_put_c_raster_row(int, CELL *);
int G_put_f_raster_row(int, FCELL *);
int G_put_d_raster_row(int, DCELL *);
int G__write_data(int, int, int);
int G__write_data_compressed(int, int, int);
int G__open_null_write(int);
int G__write_null_bits(int, unsigned char *, int, int, int);

/* put_title.c */
int G_put_cell_title(char *, char *);

/* put_window.c */
int G_put_window(struct Cell_head *);
int G__put_window(struct Cell_head *, char *, char *);

/* putenv.c */
int G_putenv(char *, char *);

/* quant.c */
void G_quant_clear(struct Quant *);
void G_quant_free(struct Quant *);
int G__quant_organize_fp_lookup(struct Quant *);
int G_quant_init(struct Quant *);
int G_quant_is_truncate(struct Quant *);
int G_quant_is_round(struct Quant *);
int G_quant_truncate(struct Quant *);
int G_quant_round(struct Quant *);
int G_quant_get_limits(struct Quant *, DCELL *, DCELL *, CELL *, CELL *);
int G_quant_nof_rules(struct Quant *);
void G_quant_get_ith_rule(struct Quant *, int, DCELL *, DCELL *, CELL *,
    CELL *);
void G_quant_set_neg_infinite_rule(struct Quant *, DCELL, CELL);
int G_quant_get_neg_infinite_rule(struct Quant *, DCELL *, CELL *);
void G_quant_set_pos_infinite_rule(struct Quant *, DCELL, CELL);
int G_quant_get_pos_infinite_rule(struct Quant *, DCELL *, CELL *);
void G_quant_add_rule(struct Quant *, DCELL, DCELL, CELL, CELL);
void G_quant_reverse_rule_order(struct Quant *);
CELL G_quant_get_cell_value(struct Quant *, DCELL);
void G_quant_perform_d(struct Quant *, DCELL *, CELL *, int);
void G_quant_perform_f(struct Quant *, FCELL *, CELL *, int);
struct Quant_table *G__quant_get_rule_for_d_raster_val(struct Quant *, DCELL);

/* quant_io.c */
int G__quant_import(char *, char *, struct Quant *);
int G__quant_export(char *, char *, struct Quant *);

/* quant_rw.c */
int G_truncate_fp_map(char *, char *);
int G_round_fp_map(char *, char *);
int G_quantize_fp_map(char *, char *, CELL, CELL);
int G_quantize_fp_map_range(char *, char *, DCELL, DCELL, CELL, CELL);
int G_write_quant(char *, char *, struct Quant *);
int G_read_quant(char *, char *, struct Quant *);

/* radii.c */
double G_meridional_radius_of_curvature(double, double, double);
double G_transverse_radius_of_curvature(double, double, double);
double G_radius_of_conformal_tangent_sphere(double, double, double);

/* range.c */
int G__remove_fp_range(char *);
int G_construct_default_range(struct Range *);
int G_read_fp_range(char *, char *, struct FPRange *);
int G_read_range(char *, char *, struct Range *);
int G_write_range(char *, struct Range *);
int G_write_fp_range(char *, struct FPRange *);
int G_update_range(CELL, struct Range *);
int G_update_fp_range(DCELL, struct FPRange *);
int G_row_update_range(CELL *, int, struct Range *);
int G__row_update_range(CELL *, int, struct Range *, int);
int G_row_update_fp_range(void *, int, struct FPRange *, RASTER_MAP_TYPE);
int G_init_range(struct Range *);
int G_get_range_min_max(struct Range *, CELL *, CELL *);
int G_init_fp_range(struct FPRange *);
int G_get_fp_range_min_max(struct FPRange *, DCELL *, DCELL *);

/* raster.c */
void *G_incr_void_ptr(void *, int);
int G_raster_cmp(void *, void *, RASTER_MAP_TYPE);
int G_raster_cpy(void *, void *, int, RASTER_MAP_TYPE);
int G_set_raster_value_c(void *, CELL, RASTER_MAP_TYPE);
int G_set_raster_value_f(void *, FCELL, RASTER_MAP_TYPE);
int G_set_raster_value_d(void *, DCELL, RASTER_MAP_TYPE);
CELL G_get_raster_value_c(void *, RASTER_MAP_TYPE);
FCELL G_get_raster_value_f(void *, RASTER_MAP_TYPE);
DCELL G_get_raster_value_d(void *, RASTER_MAP_TYPE);

/* rd_cellhd.c */
char *G__read_Cell_head(FILE *, struct Cell_head *, int);

/* reclass.c */
int G_is_reclass(char *, char *, char *, char *);
int G_is_reclassed_to(char *, char *, int *, char ***);
int G_get_reclass(char *, char *, struct Reclass *);
int G_free_reclass(struct Reclass *);
int G_put_reclass(char *, struct Reclass *);

/* remove.c */
int G_remove(char *, char *);

/* rename.c */
int G_rename(char *, char *, char *);

/* rhumbline.c */
int G_begin_rhumbline_equation(double, double, double, double);
double G_rhumbline_lat_from_lon(double);

/* rm_colr.c */
int G_remove_colr(char *);

/* set_window.c */
int G_get_set_window(struct Cell_head *);
int G_set_window(struct Cell_head *);

/* short_way.c */
int G_shortest_way(double *, double *);

/* sleep.c */
long sleep_ltp( double);
int time_ltp( double *);

/* sites.c */
int cleanse_string(char *);
char *next_att(char *);
char *G_ask_sites_old(char *, char *);
char *G_ask_sites_any(char *, char *);
char *G_ask_sites_in_mapset(char *, char *);
FILE *G_fopen_sites_old(char *, char *);
FILE *G_fopen_sites_new(char *);
int G_get_site(FILE *, double *, double *, char **);
int G_put_site(FILE *, double, double, char *);

/* squeeze.c */
char *G_squeeze(char *);

/* store.c */
char *G_store(char *);

/* strings.c */
char *G_strcpy(register char *, register char *);
char *G_chrcpy(register char *, register char *, register int);
char *G_strncpy(register char *, register char *, register int);
char *G_strmov(register char *, register char *);
char *G_chrmov(register char *, register char *, register int);
char *G_strcat(register char *, register char *);
char *G_chrcat(register char *, register char *, register int);
int G_strcasecmp(char *, char *);
char *G_strstr(char *, char *);
char *G_strdup(char *);

/* strip.c */
int G_strip(register char *);

/* support.c */
int G_open_support_old(char *, char *, char *);
FILE *G_fopen_support_old(char *, char *, char *);
int G_open_support_new(char *, char *, char *);
FILE *G_fopen_support_new(char *, char *, char *);

/* svd.c */
int G_svdcmp(double **, int, int, double *, double **);
int G_svbksb(double **, double [], double **, int, int, double [], double []);
int G_svelim(double *, int);

/* system.c */
int G_system(char *);

/* tempfile.c */
char *G_tempfile(void);
char *G__tempfile(int);
int G__temp_element(char *);

/* timestamp.c */
void G_init_timestamp(struct TimeStamp *);
void G_set_timestamp(struct TimeStamp *, DateTime *);
void G_set_timestamp_range(struct TimeStamp *, DateTime *, DateTime *);
int G__read_timestamp(FILE *, struct TimeStamp *);
int G__write_timestamp(FILE *, struct TimeStamp *);
int G_get_timestamps(struct TimeStamp *, DateTime *, DateTime *, int *);
int G_read_raster_timestamp(char *, char *, struct TimeStamp *);
int G_read_vector_timestamp(char *, char *, struct TimeStamp *);
int G_write_raster_timestamp(char *, struct TimeStamp *);
int G_write_vector_timestamp(char *, struct TimeStamp *);
int G_format_timestamp ( struct TimeStamp *, char *);
int G_scan_timestamp ( struct TimeStamp *, char *);
int G_remove_raster_timestamp (char *);
int G_remove_vector_timestamp (char *);

/* token.c */
char **G_tokenize(char *, char *);
int G_free_tokens(char **);

/* trim_dec.c */
int G_trim_decimal(char *);

/* unctrl.c */
char *G_unctrl(int);

/* unix_socks.c */
char *G_sock_get_fname(char *);
int G_sock_exists(char *);
int G_sock_bind(char *);
int G_sock_listen(int, unsigned int);
int G_sock_accept(int);
int G_sock_connect(char *);

/* view.c */
int G_3dview_warning(int);
int G_get_3dview_defaults(struct G_3dview *, struct Cell_head *);
int G_put_3dview(char *, char *, struct G_3dview *, struct Cell_head *);
int G_get_3dview(char *, char *, struct G_3dview *);

/* whoami.c */
char *G_whoami(void);

/* wind_2_box.c */
int G_adjust_window_to_box(struct Cell_head *, struct Cell_head *, int, int);

/* wind_format.c */
int G_format_northing(double, char *, int);
int G_format_easting(double, char *, int);
int G_format_resolution(double, char *, int);

/* wind_limits.c */
int G_limit_east(double *, int);
int G_limit_west(double *, int);
int G_limit_north(double *, int);
int G_limit_south(double *, int);

/* wind_overlap.c */
int G_window_overlap(struct Cell_head *, double, double, double, double);
double G_window_percentage_overlap(struct Cell_head *, double, double, double,
    double);

/* wind_scan.c */
int G_scan_northing(char *, double *, int);
int G_scan_easting(char *, double *, int);
int G_scan_resolution(char *, double *, int);

/* window_map.c */
int G__create_window_mapping(int);
double G_northing_to_row(double, struct Cell_head *);
double G_adjust_east_longitude(double, double);
double G_adjust_easting(double, struct Cell_head *);
double G_easting_to_col(double, struct Cell_head *);
double G_row_to_northing(double, struct Cell_head *);
double G_col_to_easting(double, struct Cell_head *);
int G_window_rows(void);
int G_window_cols(void);
int G__init_window(void);
int G_row_repeat_nomask(int, int);

/* wr_cellhd.c */
int G__write_Cell_head(FILE *, struct Cell_head *, int);

/* writ_zeros.c */
int G_write_zeros(int, long);

/* yes.c */
int G_yes(char *, int);

/* zero.c */
int G_zero(register void *, register int);

/* zero_cell.c */
int G_zero_cell_buf(register CELL *);
int G_zero_raster_buf(register void *, RASTER_MAP_TYPE);

/* zone.c */
int G_zone(void);

#endif /* GRASS_GISDEFS_H */
