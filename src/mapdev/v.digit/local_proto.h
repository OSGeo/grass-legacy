/*
 * $Id$
 */
 
#ifndef V_DIGIT_LOCAL_PROTO_H
#define V_DIGIT_LOCAL_PROTO_H

#include "digit.h"

/* Define ANOTHER_BUTTON to click conveniently for two button mouse.
 * Read src/CMD/head/head and do not define here for consistency.
 */
/*
#define ANOTHER_BUTTON
 */

#define	LEFTB	1

#ifndef ANOTHER_BUTTON
#	define MIDDLEB	2
#	define RIGHTB	3
#else
#	define MIDDLEB	3
#	define RIGHTB	2
#endif

/* #define ASIAN_CHARS to input asian characters.
 * Does not show characters when input due to waddch() function,
 * but characters are still valid for further use.
 *
 * If you have any problems with this, please inform me.
 * <hdcho@geni.knu.ac.kr>
 *
 * I recommend that you don't touch this source files for further CVS checkout.
 * Instead, add "DASIAN_CHARS = -DASIAN_CHARS" into grass/src/CMD/head/head
 * file. This method does not confuse CVS checkout process.
 */
/*
#define ASIAN_CHARS
 */


int print_binary_int(int);
int print_binary_char(int);
int change_mode(int);
/* add_scale.c */
int add_scale(void);
/* ask_driver.c */
int ask_yes_no(char *);
int ask_driver_yes_no(char *);
int ask_driver(void);
int get_digitizer_button_xy(double *, double *);
int ask_driver_raw(double *, double *);
int coll_a_pnt(double *, double *);
int _coll_a_pnt(double *, double *);
/* backdrop.c */
int ask_backdrop(void);
int display_backdrop(void);
int disable_backdrop(void);
int enable_backdrop(void);
int disenable_backdrop(int);
/* bin_digit.c */
int unlock_file(char *);
/* coll_pts.c and collect_p.c */
/* color.c */
int Display_color_settings(void);
int color_toggle(register int);
char *color_name(int);
/* compress.c */
int write_out(int);
/* customize.c */
int Customize(void);
/* dig_dev.c */
int D_readall(int *, int *);
int D_ask_if_err(void);
int D_get_scale(float *);
int D_end_digit(void);
/* digitize.c */
int Digitize(void);
/* digmain.c */
void close_down(int);
int digmain(char *, char *, char *, int, char *, char *);
/* drawcell.c */
int drawcell(void);
/* edit.c */
int Edit(void);
/* find_w_dig.c */
int find_node_with_dig(double *, double *, double, char *);
int find_point_with_dig(double *, double *, int, double);
int new_point_with_dig(double *, double *, char *);
int find_line_with_dig(int, char *, int (*)());
/* find_w_mouse.c */
int find_node_with_mouse(double *, double *, double, char *);
int find_point_with_mouse(double *, double *, int, double);
int new_point_with_mouse(double *, double *, char *);
int find_line_with_mouse(int, char *, int (*)());
int _find_line_with_mouse(int, char *, int (*)(), int, double, double);
/* flush.c */
int V_flush(void);
/* get_deg.c */
int get_deg(char *, int);
/* get_diginput.c */
int get_diginput(int *, int *);
/* get_point.c */
int get_point(double *, double *, char *);
int new_point_with_digitizer(double *, double *, char *);
/* gorun.c */
int gorun(char *, char *);
/* graphics.c */
int init_graphics(void);
int do_graphics(void);
/* initialize.c */
int init_colors (void);
int init_init ( FILE *,FILE *, char *, char *);
int initialize ( FILE *,FILE *, char *, char *);
int init_colors(void);
/* interact.c */
int interact(void);
int Main_info(void);
/* interface.c */
int D_cursor_buttons(void);
int D_start_button(void);
int D_foot_switch(void);
int D_clear_driver(void);
int D_ask_driver_raw(double *, double *);
int D_read_raw(int *, int *);
int delay(int);
/* label.c */
int Label(void);
int Label_settings(void);
int ask_cat(void);
/* leave.c */
int leave(void);
/* map_ask.c */
int ask_map_coor(int);
int shrink_map_coor(void);
/* map_ask_new.c */
int ask_map_coor_ll(int);
int shrink_map_coor_ll(void);
/* map_coor.c and map_coor_new.c */
int load_coor_from_file(FILE *);
int save_coor_to_file(FILE *);
/* map_init.c */
int init_map(char *);
/* map_init_new.c */
int init_map(char *);
/* map_reg.c */
int register_map_coor(int);
int show_reg_menu(void);
int get_reg_response(double *, double *);
/* map_resid.c and map_residnew.c */
int show_residual_results(int, int);
int show_coor_only(int, int, int);
/* map_scale.c */
int calculate_map_scale(void);
double get_map_scale(void);
/* measure.c */
/* menus.c */
int update_global_menu(void);
int _update_global_menu(void);
int Set_Global(int, int);
int Set_G_Mask(int, int);
int Help_main(void);
int Help_global(void);
int Help_digitize(void);
int Help_edit(void);
int Help_label(void);
int Help_window(void);
int Help_custom(void);
int Help_tool(void);
int Help_display(void);
int Help_debug(void);
int Help_color(void);
/* mk_window.c */
int set_window_w_mouse(void);
int draw_default_window(double, double, double, double);
/* mouse_coll.c */
/* mouse_yn.c */
int mouse_yes_no(char *);
int mouse_next_prev(char *);
int mouse_yes_no_zoom(char *, unsigned char, struct line_pnts *);
/* node_color.c */
int dig_node_color(int);
/* oplot.c */
int plot_points(int, int, double *, double *, int, int);
/* overlay.c */
int ask_overlay(void);
int display_overlay(void);
int disable_overlay(void);
int enable_overlay(void);
int disenable_overlay(int);
/* plot.c */
/* reset_map.c */
int check_map_explain(void);
int check_map_buttons(void);
int check_map_generic(void);
int check_map_ft_swtch(void);
/* scal_window.c */
int scal_window_w_mouse(unsigned char type, struct line_pnts *Xpoints);
/* screen_plt.c */
int First(double *, double *);
int Next(double *, double *);
int Adot(double *, double *, char *);
int Dot(double *, double *);
int Blot(double *, double *);
int _Blot(double *, double *);
int _BigBlot(double *, double *);
/* set_prior.c */
int set_priority_3b2(void);
int init_priority(void);
int set_priority(void);
int unset_priority(void);
int swap_re_uids(void);
int set_uid_to_user(void);
/* set_thresh.c */
int set_thresh(void);
int calc_snap_thresh(double);
int calc_thresh(double);
int map_to_dig_thresh(double);
/* set_window.c */
int init_window(void);
int set_window_w(void);
/* slid_window.c */
int slid_window_w_mouse(unsigned char type, struct line_pnts *Xpoints);
/* snap.c */
int near_zero(double);
/* states.c */
int init_states(void);
int set_default_display(void);
/* str.c */
long strsplit(char [], int, char [], char []);
/* toolbox.c */
int Toolbox(void);
/* tty.c */
int Get_old_tty(void);
int Get_new_tty(void);
int Old_tty(void);
int New_tty(void);
/* wind_conv.c */
int window_conversions(double, double, double, double);
int utm_to_screen(double, double, int *, int *);
int screen_to_utm(int, int, double *, double *);
int outline_window(void);
int get_D_west(void);
int get_D_east(void);
int get_D_north(void);
int get_D_south(void);
/* window.c */
int Window(void);
int expand_window(double, double, double, double, int);
int move_window(double, double, double, double, int);
int clear_window(void);
int erase_window(void);
int Save_Disp_settings(void);
int Restore_Disp_settings(void);
int Zero_Disp_settings(void);
int zoom_window(unsigned char type, struct line_pnts *Xpoints);
int slid_window(unsigned char type, struct line_pnts *Xpoints);
int scal_window(unsigned char type, struct line_pnts *Xpoints);
#ifdef DIG___STRUCTS___
int area_outside_window (P_AREA *);
int line_outside_window (P_LINE *);
int _line_in_window ( P_LINE *,double,double,double,double);
int line_in_window (P_LINE *);
#endif
/* window_rout.c */
int window_rout(double, double, double, double);
int fit_window(double *, double *, double *, double *);

#ifdef GRASS_DIGHEAD_H
/* head_info.c */
int get_head_info(int, struct dig_head *);
#endif

#ifdef GRASS_GIS_H
/* eq.c */
int eq_grey_colors (char *,char *,struct Colors *,int);
#endif

#endif
