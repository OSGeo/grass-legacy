#ifndef _GRASS_RASTER_H
#define _GRASS_RASTER_H

#include <grass/monitors.h>
#include <grass/freetypecap.h>

/* commands.c */
int R_flush(void);
void R_set_update_function(int(*fnc)(int,int));
int R_call_update_function(int,int);
int R_has_update_function();
void R_set_cancel(int);
int R_get_cancel(void);
int R_raster(int,int,int,const int *);

/* get.c */
int R_get_location_with_box(int,int,int *,int *,int *);
int R_get_location_with_box_old(int,int,int *,int *,int *);
int R_get_location_with_line(int,int,int *,int *,int *);
int R_get_location_with_line_old(int,int,int *,int *,int *);
int R_get_location_with_pointer(int *,int *,int *);
int R_get_location_with_pointer_old(int *,int *,int *);

/* io.c */
int _send_ident(int);
int _send_char(const unsigned char *);
int _send_char_array(int,const unsigned char *);
int _send_int_array(int,const int *);
int _send_float_array(int,const float *);
int _send_int(const int *);
int _send_float(const float *);
int _send_text(const char *);

int _get_char(char *);
int _get_int(int *);
int _get_float(float *);
int _get_text(char *);
char *_get_text_2(void);

int R__open_quiet(void);
int sync_driver(char *name);
int _hold_signals(int);
int R_stabilize(void);
int R_kill_driver(void);
int R_close_driver(void);
int R_release_driver(void);

/* io_fifo.c / io_sock.c */
int unlock_driver(int);
int R_open_driver(void);

/* pad.c */
int R_pad_freelist(char **,int);
int R_pad_create(const char *);
int R_pad_current(char *);
int R_pad_delete(void);
int R_pad_invent(char *);
int R_pad_list(char ***,int *);
int R_pad_select(const char *);
int R_pad_append_item(const char *,const char *,int);
int R_pad_delete_item(const char *);
int R_pad_get_item(const char *,char ***,int *);
int R_pad_list_items(char ***,int *);
int R_pad_set_item(const char *,const char *);
int R_pad_perror(const char *,int);

/* parse_mon.c */
struct MON_CAP *R_parse_monitorcap(int,char *);

/* parse_ft.c */
struct FT_CAP *R_parse_freetypecap(void);
void R_free_freetypecap(struct FT_CAP *);

/* protocol.c */
int R_screen_left(void);
int R_screen_rite(void);
int R_screen_bot(void);
int R_screen_top(void);
int R_get_num_colors(int *);

int R_color_table_float(void);
int R_color_table_fixed(void);
int R_color_offset(int);

int R_color(int);
int R_standard_color(int);
int R_RGB_color(unsigned char,unsigned char,unsigned char);
int R_reset_color(unsigned char,unsigned char,unsigned char,int);
int R_reset_colors(int,int,unsigned char *,unsigned char *,unsigned char *);

int R_line_width(int);
int R_erase(void);

int R_move_abs(int,int);
int R_move_rel(int,int);
int R_cont_abs(int,int);
int R_cont_rel(int,int);
int R_polydots_abs(int *,int *,int);
int R_polydots_rel(int *,int *,int);
int R_polyline_abs(int *,int *,int);
int R_polyline_rel(int *,int *,int);
int R_polygon_abs(int *,int *,int);
int R_polygon_rel(int *,int *,int);
int R_box_abs(int,int,int,int);
int R_box_rel(int,int);

int R_text_size(int,int);
int R_text_rotation(float);
int R_set_window(int,int,int,int);
int R_text(const char *);
int R_get_text_box(const char *,int *,int *,int *,int *);

int R_font(const char *);
int R_font_freetype(const char *);
int R_charset(const char *);
int R_font_freetype_release(void);

int R_panel_save(const char *name,int,int,int,int);
int R_panel_restore(const char *name);
int R_panel_delete(const char *name);

int R_set_RGB_color(unsigned char *,unsigned char *,unsigned char *);
int R_RGB_raster(int,int,unsigned char *,unsigned char *,unsigned char *,unsigned char *);
int R_raster_char(int,int,int,const unsigned char *);
int R_raster_int(int,int,int,const int *);
int R_bitmap(int,int,int,const unsigned char *);

#endif
