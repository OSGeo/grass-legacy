/* alloc_pats.c */
int allocate_pattern_array(void);
/* builtin.c */
int builtin_patterns(void);
/* case.c */
int lowercase(char *);
int uppercase(char *);
/* cat_color.c */
int cat_color_num(int);
/* cellfile.c */
int cellfile(char *, char *);
/* chars.c */
/* chk_scale.c */
int check_scale(char *);
/* color.c */
int set_colormode(char *);
int build_color_tables(void);
int build_dither_tables(void);
int dump_color_tables(void);
int printer_color_number(int,int,int);
int red_carryover(int);
int grn_carryover(int);
int blu_carryover(int);
int red_dither(unsigned char *, int, int, int);
int grn_dither(unsigned char *, int, int, int);
int blu_dither(unsigned char *, int, int, int);
/* comment.c */
int commentfile(char *);
/* cont_move.c */
int move_abs(int, int);
int cont_abs(int, int);
/* diamond.c */
int diamond(int, int);
/* distance.c */
double distance(double, double);
/* do_grid.c */
int do_grid(void);
int do_grid_numbers(int);
/* do_labels.c */
int do_labels(int);
/* do_plfile.c */
int do_plfile(int);
/* do_sites.c */
int do_sites(void);
/* do_vectors.c */
int do_vectors(int);
/* dot.c */
int dot(int, int);
#ifdef GRASS_ICON_H
/* draw_icon.c */
int draw_icon(ICON *, int, int);
#endif
/* draw_line.c */
int draw_line(int, int, int, int);
/* draw_text.c */
int draw_text(char *, int, int);
int text_line(int, int, int, int);
int set_text_background(int);
int set_text_border(int);
int set_text_color(int);
int set_text_rotation(int);
int set_text_size(double);
int set_text_width(int);
int set_text_xref(int);
int set_text_yref(int);
int set_text_scale(double);
int set_text_hwidth(int);
int set_text_hcolor(int);
int set_reasonable_text_size(void);
/* error.c */
int error(char *, char *, char *);
/* font.c */
char *fontfilename(char *, char *);
int check_font(char *);
int select_font(char *);
int select_standard_font(void);
int list_fonts(void);
int get_font_char(int, int *, char **, char **);
/* getcats.c */
int getcats(void);
/* getgrid.c */
int getgrid(void);
/* graph_line.c */
int graph_line(int, int, int, int);
/* graph_pnt.c */
int graph_point(int, int);
/* graph_text.c */
int graph_char(int *, int *, double, double, int);
int graph_text(int *, int *, double, double, char *);
/* header.c */
int header(int, int, char *);
/* input.c */
int input(int, char *, char *[]);
int gobble_input(void);
/* input_pat.c */
int input_pattern(char *);
/* key_data.c */
int key_data(char *, char **, char **);
/* label.c */
int record_label(char *, char *, char *);
/* lblfile.c */
int labelfile(char *, char *);
/* line_eq.c */
int line_eq(int, int, int, int, int, int);
/* line_style.c */
int set_line_style_solid(void);
int set_line_style(char *, int [9]);
int regress_line_style(void);
#ifdef GRASS_GIS_H
/* ctable.c */
int ctable(struct Categories *, struct Colors *, struct Cell_stats *);
int do_label(FILE *, int);
/* lookup.c */
int lookup_from_pattern(CELL, int, int);
/* map.c */
int map(struct Cell_head *);
/* mask_vctrs.c */
int mask_vectors(CELL *);
/* parse_list.c */
int parse_val_list(char *, DCELL **);
/* scale.c */
int scale(struct Cell_head *, int, int, char *);
int unscaled(struct Cell_head *, int, int);
int print_session(FILE *);
int set_graphics(struct Cell_head *, int, int);
/* trailer.c */
int trailer(struct Cell_head *, int, char *);
#endif
/* main.c */
int usage(int);
/* outline.c */
int outlinefile(void);
/* parms.c */
/* patcc.c */
int main(int, char *[]);
/* patcolor.c */
int scan_patcolor(char *, int *, int *);
/* pattern.c */
int add_pattern_to_list(char *, long, int, int, int, int);
int find_pattern(char *, long *, int *, int *, int *, int *);
int begin_pattern(char *);
int store_pattern(char *);
int end_pattern(void);
int any_patterns(void);
int open_pattern_file(int, int);
int close_pattern_file(void);
int unlink_pattern_file(void);
/* plfile.c */
int record_point(double, double);
int record_line(double, double, double, double);
int add_to_plfile(char *);
/* predef_pat.c */
int read_predefined_patterns(void);
/* scan_color.c */
int scan_color(char *, int *, int *, int *, int *);
/* scan_gis.c */
int scan_gis(char *, char *, char *, char *, char *, char *, int);
/* scan_misc.c */
int scan_easting(char *, double *);
int scan_northing(char *, double *);
int scan_resolution(char *, double *);
/* scan_ref.c */
int scan_ref(char *, int *, int *);
/* session.c */
int add_to_session(int, char *);
int accept(void);
int reject(void);
/* set_color.c */
int set_color(int);
/* set_grphcs.c */
int init_graphics(int);
/* set_pat.c */
int set_pattern(int, char *);
int set_all_patterns(void);
/* set_width.c */
int set_width(int);
/* sitefile.c */
int sitefile(char *, char *);
/* sitesfile.c */
/* title.c */
int title(char *);
/* vectdot.c */
int vect_type_is_dot(int);
/* vectfile.c */
int vectfile(char *, char *);
/* vectr_info.c */
int vector_info(int);
int vector_info_lines(int);
/* windfile.c */
int windfile(char *, char *);
/* window_cnv.c */
int window_row(double);
int window_col(double);
/* window_lne.c */
int window_point(double, double);
int window_line(double, double, double, double);
/* yesno.c */
int yesno(char *, char *);
