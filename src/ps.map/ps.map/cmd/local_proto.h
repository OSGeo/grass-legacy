/* bound.c */
/* box_stuff.c */
int box_path(double, double, double, double);
int box_clip(double, double, double, double);
int box_fill(double, double, double, double, int);
int box_draw(double, double, double, double);
/* cellfile.c */
int cellfile(char *, char *);
/* chk_scale.c */
int check_scale(char *);
/* comment.c */
int commentfile(char *);
int do_comments(void);
/* ctablfile.c */
int ctablfile(void);
/* distance.c */
double distance(double, double);
/* do_grid.c */
int do_grid(void);
int do_grid_numbers(void);
/* do_header.c */
int do_map_header(char *);
int read_header_file(char *);
/* do_labels.c */
int do_labels(int);
#ifdef _STDIO_H
int do_label(FILE *);
#endif
/* do_masking.c */
int do_masking(void);
/* do_plfile.c */
int do_plfile(int);
/* do_psfiles.c */
int do_psfiles(void);
/* do_sites.c */
int do_sites(void);
/* do_vectors.c */
int do_vectors(int);
/* error.c */
int error(char *, char *, char *);
/* fit_map.c */
int fit_map_to_box(void);
/* get_font.c */
int get_font(char *);
/* getgrid.c */
int getgrid(void);
/* gprims.c */
int draw_line(double, double, double, double);
int start_line(double, double);
int move_local(int, int);
int cont_local(int, int);
int set_line_width(double);
int set_font_name(char *);
int set_font_size(int);
int show_text(double, double, char *);
/* groupfile.c */
int groupfile(void);
/* hdrfile.c */
int hdrfile(void);
/* infofile.c */
int infofile(void);
/* input.c */
int input(int, char *, char *[]);
int gobble_input(void);
/* key_data.c */
int key_data(char *, char **, char **);
/* label.c */
int record_label(char *, char *, char *);
/* lblfile.c */
int labelfile(char *, char *);
/* main.c */
int usage(int);
/* makeprocs.c */
int make_procs(void);
/* map_info.c */
int map_info(void);
/* map_setup.c */
int map_setup(void);
/* mtextbox.c */
int multi_text_box_path(double, double, int, int, char *, int, float);
int multi_lines(char *);
/* outl_io.c */
int o_io_init(void);
int o_read_row(void *);
#ifdef GRASS_GIS_H
RASTER_MAP_TYPE o_open_file(char *);
#endif
int o_close_file(void);
char *xmalloc(int, char *);
int xfree(char *, char *);
char *xrealloc(char *, int, char *);
#ifdef GRASS_GIS_H
/* parse_list.c */
int parse_val_list(char *, DCELL **);
#endif
/* plfile.c */
int record_point(double, double);
int record_line(double, double, double, double);
int record_rectangle(double, double, double, double);
int record_eps(double, double);
int add_to_plfile(char *);
/* ps_clrtbl.c */
int ps_colortable(void);
/* ps_colors.c */
int get_color_number(char *);
int get_color_rgb(int, float *, float *, float *);
int color_name_is_ok(char *);
char *get_color_name(int);
int set_rgb_color(int);
/* ps_header.c */
int write_PS_header(void);
int write_bounding_box(void);
/* ps_map.c */
int ps_map(void);
/* ps_outline.c */
int ps_outline(void);
int outlinefile(void);
int draw_outline(void);
int o_alloc_bufs(int, int);
int draw_top(void);
int draw_rite(void);
int draw_left(void);
int draw_bot(void);
#ifdef GRASS_VECT_H
/* ps_vector.c */
int PS_vector_plot(struct Map_info *, int, int);
/* ps_area.c */
int PS_area_plot(struct Map_info *, int);
/* vect.c */
struct line_pnts *parallel_line(struct line_pnts *, double, double);
int adjust_line(struct line_pnts *);
void reverse_line(struct line_pnts *);
int construct_path(struct line_pnts *, double, int);
#endif
/* rast_plot.c */
int PS_make_mask(void);
int PS_raster_plot(void);
#ifdef GRASS_GIS_H
int ps_write_mask_row(register CELL *);
#endif
/* read_cfg.c */
int read_cfg(void);
/* scale.c */
double scale(char *);
/* scan_gis.c */
int scan_gis(char *, char *, char *, char *, char *, char *, int);
/* scan_misc.c */
int scan_easting(char *, double *);
int scan_northing(char *, double *);
int scan_resolution(char *, double *);
/* scan_ref.c */
int scan_ref(char *, int *, int *);
int lowercase(register char *);
/* session.c */
int add_to_session(int, char *);
int accept(void);
int reject(void);
#ifdef _STDIO_H
int print_session(FILE *);
#endif
/* show_scale.c */
int show_scale(void);
/* sitefile.c */
int sitefile(char *, char *);
/* textbox.c */
int text_box_path(double, double, int, int, char *, int, float);
/* vectfile.c */
int vectfile(char *, char *);
/* vlegend.c */
int vect_legend(void);
/* vlegfile.c */
int vlegfile(void);
/* windfile.c */
int windfile(char *, char *);
/* yesno.c */
int yesno(char *, char *);
/* eps.c */
int eps_bbox (char *, double *, double *, double *, double *);
int eps_trans (double, double, double, double, double, double, double, double, double *, double *);
#ifdef _STDIO_H
int eps_save (FILE*,char*,char*);
int eps_draw_saved (FILE *, char *, double, double, double, double);
int eps_draw (FILE *, char *, double, double, double, double);
#endif
