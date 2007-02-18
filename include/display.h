#ifndef GRASS_DISPLAY_H
#define GRASS_DISPLAY_H

#include <grass/gis.h>
#include <grass/symbol.h>

/* clip.c */
int D_clip(register double, register double, register double, register double, register double *, register double *, register double *, register double *);
/* cnversions.c */
int D_do_conversions(struct Cell_head *, int, int, int, int);
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
void D_get_u(double [2][2]);
void D_get_a(int [2][2]);
void D_get_d(int [2][2]);
/* color_list.c */
char *D_color_list(void);
/* draw.c */
int D_set_clip_window(int, int, int, int);
int D_set_clip_window_to_map_window(void);
int D_set_clip_window_to_screen_window(void);
int D_cont_abs(int, int);
int D_cont_rel(int, int);
int D_move_abs(int, int);
int D_move_rel(int, int);
/* draw2.c */
void D_set_clip(double, double, double, double);
void D_clip_to_map(void);
void D_move_clip(double, double);
int D_cont_clip(double, double);
void D_polydots_clip(const double *, const double *, int);
void D_polyline_clip(const double *, const double *, int);
void D_polygon_clip(const double *, const double *, int);
void D_box_clip(double, double, double, double);
void D_move(double, double);
void D_cont(double, double);
void D_polydots(const double *, const double *, int);
void D_polyline(const double *, const double *, int);
void D_polygon(const double *, const double *, int);
void D_box(double, double, double, double);
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
int D_get_list(char ***, int *);
int D_clear_window(void);
int D_set_erase_color(char *);
int D_get_erase_color(char *);
/* popup.c */
int D_popup(int, int, int, int, int, int, char *[]);
/* raster.c */
int D_draw_raster(int, void *, struct Colors *, RASTER_MAP_TYPE);
int D_draw_d_raster(int, DCELL *, struct Colors *);
int D_draw_f_raster(int, FCELL *, struct Colors *);
int D_draw_c_raster(int, CELL *, struct Colors *);
int D_draw_cell(int, CELL *, struct Colors *);
int D_cell_draw_setup(int, int, int, int);
/* raster_rgb.c */
int D_draw_raster_RGB(int, void *, void *, void *,
		      struct Colors *, struct Colors *, struct Colors *,
		      RASTER_MAP_TYPE, RASTER_MAP_TYPE, RASTER_MAP_TYPE);
int D_draw_d_raster_RGB(int, DCELL *, DCELL *, DCELL *,
			struct Colors *, struct Colors *, struct Colors *);
int D_draw_f_raster_RGB(int, FCELL *, FCELL *, FCELL *,
			struct Colors *, struct Colors *, struct Colors *);
int D_draw_c_raster_RGB(int,  CELL *,  CELL *,  CELL *,
			struct Colors *, struct Colors *, struct Colors *);
int D_draw_cell_RGB(int, CELL *, CELL *, CELL *,
		    struct Colors *, struct Colors *, struct Colors *);
int D_cell_draw_setup_RGB(int, int, int, int);
int D_raster_of_type_RGB(void *, void *, void *,
			 int, int,
			 struct Colors *, struct Colors *, struct Colors *,
			 RASTER_MAP_TYPE, RASTER_MAP_TYPE, RASTER_MAP_TYPE);
/* raster2.c */
int D_set_overlay_mode(int);
int D_color(CELL, struct Colors *);
int D_c_color(CELL, struct Colors *);
int D_d_color(DCELL, struct Colors *);
int D_f_color(FCELL, struct Colors *);
int D_color_of_type(void *, struct Colors *, RASTER_MAP_TYPE);
int D_raster_of_type(void *, int, int, struct Colors *, RASTER_MAP_TYPE);
int D_raster(CELL *, int, int, struct Colors *);
int D_c_raster(CELL *, int, int, struct Colors *);
int D_f_raster(FCELL *, int, int, struct Colors *);
int D_d_raster(DCELL *, int, int, struct Colors *);
/* scan_dbl.c */
int D_scan_double(char *, double *);
/* scan_float.c */
int D_scan_float(char *, float *);
/* scan_int.c */
int D_scan_int(char *, int *);
/* setup.c */
int D_setup(int);
/* symbol.c */
int D_symbol(SYMBOL *, int, int, RGBA_Color *, RGBA_Color *);
/* tran_colr.c */
int D_translate_color(const char *);
int D_translate_or_add_color(const char *, int);
int D_allocate_color(void);
int D_parse_color(const char *, int);
int D_raster_use_color(int);
/* window.c */
int D_new_window(char *, int, int, int, int);
int D_new_window_percent(char *, float, float, float, float);
int D_set_cur_wind(char *);
int D_get_cur_wind(char *);
int D_show_window(int);
int D_get_screen_window(int *, int *, int *, int *);
int D_check_map_window(struct Cell_head *);
int D_reset_screen_window(int, int, int, int);
int D_timestamp(void);
void D_remove_window(void);
void D_erase_window(void);
void D_erase(char *);
void D_remove_windows(void);
void D_full_screen(void);

#endif /* GRASS_DISPLAY_H */
