#ifndef _GRASS_GIS_LIB_
#include "gis.h"
#endif
#ifndef _GRASS_DISPLAY_LIB_
#define _GRASS_DISPLAY_LIB_
/* clip.c */
int D_clip(register double, register double, register double, register double, register double *, register double *, register double *, register double *);
/* cnversions.c */
int D_do_conversions(struct Cell_head *, int, int, int, int);
double D_get_u_to_a_yconv(void);
double D_get_u_to_a_xconv(void);
double D_get_a_to_d_xconv(void);
double D_get_a_to_d_yconv(void);
double D_get_u_to_d_xconv(void);
double D_get_u_to_d_yconv(void);
double D_get_u_west(void);
double D_get_u_east(void);
double D_get_u_north(void);
double D_get_u_south(void);
double D_get_a_west(void);
double D_get_a_east(void);
double D_get_a_north(void);
double D_get_a_south(void);
double D_get_d_west(void);
double D_get_d_east(void);
double D_get_d_north(void);
double D_get_d_south(void);
double D_u_to_a_row(double);
double D_u_to_a_col(double);
double D_a_to_d_row(double);
double D_a_to_d_col(double);
double D_u_to_d_row(double);
double D_u_to_d_col(double);
double D_d_to_u_row(double);
double D_d_to_u_col(double);
double D_d_to_a_row(double);
double D_d_to_a_col(double);
double D_get_ns_resolution(void);
double D_get_ew_resolution(void);
/* color_list.c */
char *D_color_list(void);
/* draw.c */
int D_set_clip_window(int, int, int, int);
int D_set_clip_window_to_map_window(void);
int D_cont_abs(int, int);
int D_cont_rel(int, int);
int D_move_abs(int, int);
int intD_move_rel(int, int);
/* get_win.c */
int get_win_w_mouse(float *, float *, float *, float *);
/* ident_win.c */
int ident_win(char *);
/* list.c */
int D_set_cell_name(char *);
int D_get_cell_name(char *);
int D_set_dig_name(char *);
int D_get_dig_name(char *);
int D_set_site_name(char *);
int D_get_site_name(char *);
int D_add_to_cell_list(char *);
int D_get_cell_list(char ***, int *);
int D_add_to_dig_list(char *);
int D_get_dig_list(char ***, int *);
int D_add_to_site_list(char *);
int D_get_site_list(char ***, int *);
int D_add_to_list(char *);
int D_clear_window(void);
int D_claim_offset_is(int);
int D_offset_is(int *);
int D_set_erase_color(char *);
int D_get_erase_color(char *);
/* popup.c */
int D_popup(int, int, int, int, int, int, char *[]);
/* raster.c */
int D_draw_raster(int, void *, struct Colors *, RASTER_MAP_TYPE);
int D_draw_d_raster(int, DCELL *, struct Colors *);
int D_draw_f_raster(int, DCELL *, struct Colors *);
int D_draw_c_raster(int, DCELL *, struct Colors *);
int D_draw_cell(int, CELL *, struct Colors *);
int D_cell_draw_setup(int, int, int, int);
/* raster2.c */
int D_set_overlay_mode(int);
int D_set_colors(struct Colors *);
int D_reset_color(CELL, int, int, int);
int D_color(CELL, struct Colors *);
int D_c_color(CELL, struct Colors *);
int D_d_color(DCELL, struct Colors *);
int D_f_color(FCELL, struct Colors *);
int D_color_of_type(void *, struct Colors *, RASTER_MAP_TYPE);
int D_lookup_colors(CELL *, int, struct Colors *);
int D_lookup_c_raster_colors(CELL *, int, struct Colors *);
int D_lookup_f_raster_colors(FCELL *, int *, int, struct Colors *);
int D_lookup_d_raster_colors(DCELL *, int *, int, struct Colors *);
int D_lookup_raster_colors(void *, int *, int, struct Colors *, RASTER_MAP_TYPE);
int D_raster_of_type(void *, int, int, struct Colors *, RASTER_MAP_TYPE);
int D_raster(CELL *, int, int, struct Colors *);
int D_c_raster(CELL *, int, int, struct Colors *);
int D_f_raster(FCELL *, int, int, struct Colors *);
int D_d_raster(DCELL *, int, int, struct Colors *);
int D_check_colormap_size(CELL, CELL, int *);
/* scan_dbl.c */
int D_scan_double(char *, double *);
/* scan_float.c */
int D_scan_float(char *, float *);
/* scan_int.c */
int D_scan_int(char *, int *);
/* setup.c */
int D_setup(int);
/* tran_colr.c */
int D_translate_color(char *);
/* window.c */
int D_new_window(char *, int, int, int, int);
int D_set_cur_wind(char *);
int D_get_cur_wind(char *);
int D_show_window(int);
int D_get_screen_window(int *, int *, int *, int *);
int D_check_map_window(struct Cell_head *);
int D_reset_screen_window(int, int, int, int);
int D_timestamp(void);
int D_remove_window(void);
int D_erase_window(void);
#endif
