/* GLcolormap.c */
void installcolormap(data_cell *);
/* GLx.c */
void swap_buffers(void);
void show_it(Widget, data_cell *, caddr_t *);
/* animation_dyn.c */
/* animation_panel.c */
void animation_panel(Widget, data_cell *, caddr_t);
/* before_pops.c */
void before_pops(Widget, data_cell *, caddr_t);
/* change_view.c */
void change_xypos(Widget, data_cell *, XEvent *);
int update_ranges(data_cell *);
/* check.c */
void check_space(data_cell *, int, int);
/* clear.c */
void cleartxt(Widget, data_cell *, caddr_t);
/* close_me.c */
void close_me(Widget, data_cell *, caddr_t);
void close_destroy_me(Widget, data_cell *, caddr_t);
/* closeb.c */
void closeb(int, data_cell *, int);
/* color_panel.c */
void color_panel(Widget, data_cell *, caddr_t);
/* cplane_panel.c */
int init_cp_labels(void);
void fcolor_above(Widget, data_cell *, caddr_t *);
void fcolor_below(Widget, data_cell *, caddr_t *);
void fcolor_blend(Widget, data_cell *, caddr_t *);
void fcolor_grey(Widget, data_cell *, caddr_t *);
void fcolor_off(Widget, data_cell *, caddr_t *);
void cplane_rotate(Widget, data_cell *, caddr_t *);
void cplane_translate(Widget, data_cell *, caddr_t *);
void set_cp_widgets(data_cell *);
void cplane_panel(Widget, data_cell *, caddr_t);
/* create_menu.c */
void xman_create_menu_items(char *, Widget, xman_menu_struct *, int);
/* draw.c */
int bgntime(void);
int endtime(void);
int enable_cxl(Widget);
void check_button(void);
int surf_all_draw(data_cell *);
int vect_all_draw(data_cell *);
int site_all_draw(data_cell *);
int curvect_quick_draw(data_cell *);
/* drawpuck.c */
void drawpuck(Widget, data_cell *, caddr_t *);
/* img_dump.c */
/* info.c */
int init_inform(void);
void inform(data_cell *, char *);
/* init.c */
int init1(data_cell *);
int init2(data_cell *);
int init3(data_cell *);
void init_atts(data_cell *);
int att_set_status(data_cell *, int, int);
int init_dm_settings(data_cell *);
int init_lights(data_cell *);
int set_default_wirecolors(data_cell *, int);
int sort_surfs_mid(int *, int *, int *, int);
int sort_surfs_max(int *, int *, int *, int);
/* lgtexp.c */
void lgtexp(Widget, data_cell *, caddr_t);
/* light_dyn.c */
void position_light_xy(Widget, data_cell *, XEvent *);
/* light_panel.c */
void light_panel(Widget, data_cell *, caddr_t);
int set_light_sliders(data_cell *);
/* load_colors.c */
void load_colors(data_cell *);
/* main.c */
int main(int, char *[]);
int parse_command(data_cell *, int, char **);
/* make_arrows.c */
Widget make_arrows(Widget, char *, int, int, int, arrow_pair *, Arg *, int *);
/* make_fsbox.c */
Widget make_fsbox(Widget, char *, char *, char *, char *, XtCallbackProc, data_cell *);
/* make_position.c */
Widget make_position(Widget, char *, void (*)(void), void (*)(void), void (*)(void), void (*)(void), data_cell *, Arg [30], int);
/* make_slider.c */
Widget make_slider(Widget, int, int, int, int, char *, void (*)(void), void (*)(void), data_cell *, int, int, Arg [30], int);
/* make_text.c */
Widget make_text(Widget, char *, int, char *, void (*)(void), data_cell *, Arg [], int);
/* make_title.c */
Widget make_title(int, data_cell *, int);
/* pop_attr.c */
void pop_attr(Widget, data_cell *, caddr_t);
/* pop_color.c */
void pop_color(Widget, data_cell *, caddr_t);
int update_rgb(data_cell *);
int set_color_prompts(void);
int set_palette_colors(Display *, Colormap);
/* pop_transl.c */
void set_crosshair(double, double);
void draw_crosshair(Widget, data_cell *, caddr_t *);
void trackmouse2(Widget, data_cell *, XEvent *);
void exp2(Widget, data_cell *, caddr_t);
void set_trans_widgets(data_cell *);
void pop_transl(Widget, data_cell *, caddr_t);
/* pops.c */
void pops(Widget, data_cell *, int);
void nyi_pops(Widget, data_cell *, caddr_t);
void bf_unavail(Widget, data_cell *, caddr_t);
/* pos.c */
/* primary_controls.c */
void make_primary_controls(data_cell *, int, char *[]);
void exit_now(Widget, data_cell *, caddr_t);
int set_main_sliders(data_cell *);
/* quick_draw.c */
int quick_draw(data_cell *);
int cplane_draw(int, data_cell *);
/* reshow.c */
void reshow(data_cell *, int, int);
/* separate.c */
void separate(Widget, data_cell *, caddr_t);
/* simp_menu.c */
int init_simple_menudata(data_cell *);
void set_cur_cplane(Widget, int, caddr_t);
void set_active_cplanes(Widget, int, caddr_t);
Widget make_simple_options(Widget, char *, char *, char *[], int, int, void (*)(void), Arg [], int);
Widget make_simple_pupcheckbox(Widget, char *, char *, char *[], int, int, void (*)(void), Arg [], int);
/* site_dyn.c */
char **s_labels(data_cell *);
int load_new_site(data_cell *, char *, int);
int update_Sscroll_options(data_cell *);
int _update_Sscroll_options(data_cell *);
int update_site_options(data_cell *);
int _update_site_options(data_cell *);
void site_translate(Widget, data_cell *, XEvent *);
int pack_siteinfo(site_dm [MAX_SITES], int [MAX_SITES]);
/* sites_panel.c */
void sites_panel(Widget, data_cell *, caddr_t);
/* surf_dyn.c */
int load_new_surface(data_cell *, char *, double);
void cursurf_set_curatts(data_cell *);
int surf_init_displaymode(data_cell *, int, char *, int, int, int, int, double, double, double, double);
void surf_translate(Widget, data_cell *, XEvent *);
int pack_surfinfo(att_info [MAX_SURFS][MAX_ATTS], surf_dm [MAX_SURFS], int [MAX_SURFS]);
int update_surface_options(data_cell *);
int _update_surface_options(data_cell *);
char **sf_labels(data_cell *);
int surf_indexof_handle(data_cell *, int);
/* surface_panel.c */
void surface_panel(Widget, data_cell *, caddr_t);
/* targa_ext.c */
/* tga_out.c */
int targa_out(char *);
int do_rgb(FILE *, int, int);
int targahead_rgb(FILE *, int, int);
int targatail_rgb(FILE *);
int fwrite_short(register FILE *, int);
int fwrite_long(register FILE *, int);
int targa_extension(FILE *);
/* toggle.c */
void toggle(Widget, data_cell *, caddr_t);
/* trackmouse.c */
void trackmouse(Widget, data_cell *, XEvent *);
/* update_arrows.c */
/* update_color.c */
void update_color(Widget, data_cell *, caddr_t);
/* update_sliders.c */
int reset_slider_range(double, data_cell *, int);
void set_slider_txt(data_cell *, int);
int init_default_slider_vals1(data_cell *, float *, float *, float *);
int init_default_slider_vals2(data_cell *, float *, float *, float *);
/* utils.c */
int rgb_int_from_pix(data_cell *, unsigned long);
int set_button_colors(Widget, data_cell *, int);
int unset_button_colors(Widget, data_cell *);
void change_label(Widget, char *);
void set_pixel_color(data_cell *, unsigned long, int, int, int);
void get_pixel_color(data_cell *, unsigned long, int *, int *, int *);
unsigned long get_default_draw_color(data_cell *);
void SetPositionArgs(Arg [], int *, int, int, int, int, XtArgVal);
int make_red_yellow_ramp(int *, int, int, int);
int pack_handles(int *, int);
int _set_shortname(char *, char *);
/* vect_dyn.c */
char **v_labels(data_cell *);
int load_new_vector(data_cell *, char *, int);
int update_Vscroll_options(data_cell *);
int _update_Vscroll_options(data_cell *);
int update_vector_options(data_cell *);
int _update_vector_options(data_cell *);
void vect_translate(Widget, data_cell *, XEvent *);
int pack_vectinfo(vect_dm [MAX_VECTS], int [MAX_VECTS]);
/* vector_panel.c */
void vector_panel(Widget, data_cell *, caddr_t);
/* whats_here.c */
void whats_here_panel(Widget, data_cell *, caddr_t);
