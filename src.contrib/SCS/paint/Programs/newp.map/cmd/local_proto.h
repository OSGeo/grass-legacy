/* alloc_pats.c */
int allocate_pattern_array(void);
/* barscale.c */
int record_barscale(char *, char *);
/* builtin.c */
int builtin_patterns(void);
/* case.c */
int lowercase(register char *);
int uppercase(register char *);
/* cat_color.c */
int cat_color_num(register int);
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
int printer_color_number(register int, register int, register int);
int red_carryover(register int);
int grn_carryover(register int);
int blu_carryover(register int);
int red_dither(unsigned char *, int, int, int);
int grn_dither(unsigned char *, int, int, int);
int blu_dither(unsigned char *, int, int, int);
/* colrtable.c */
int record_ctable(void);
/* comment.c */
int commentfile(char *);
/* cont_move.c */
int move_abs(int, int);
int cont_abs(int, int);
/* ctable.c */
int ctable(struct Categories *, struct Colors *, struct Cell_stats *);
/* diamond.c */
int diamond(int, int);
/* distance.c */
double distance(double, double);
/* do_barscale.c */
int do_barscales(struct Colors *);
int do_barscale(FILE *);
/* do_cat.c */
int do_cats(struct Colors *);
int do_cat(FILE *, struct Colors *);
/* do_grid.c */
int do_grid(void);
int do_grid_numbers(int);
/* do_labels.c */
int do_labels(int);
int do_label(FILE *, int);
/* do_legend.c */
int do_legends(void);
/* do_newgrid.c */
int do_newgrid(void);
/* do_plfile.c */
int do_plfile(int);
/* do_ramp.c */
int do_ramps(struct Colors *, struct Categories *, struct Cell_stats *);
int do_ramp(FILE *, struct Colors *, struct Categories *, struct Cell_stats);
/* do_region.c */
int do_region(int);
/* do_rscale.c */
int do_rscale(void);
/* do_sites.c */
int do_sites(void);
/* do_vectors.c */
int do_vectors(int);
/* dobackground.c */
void dobg(int, int, int, int, int, int, int, int, int);
/* dot.c */
int dot(int, int);
/* draw_barscale.c */
int draw_barscale(FILE *, int, int, int, char *, double, double, int);
/* draw_cat.c */
int draw_cat(double, double, int, int, int, struct Colors *);
/* draw_dashbarscale.c */
int draw_barscale(FILE *, int, int, int, char *, double, double, int);
/* draw_icon.c */
int draw_icon(ICON *, int, int);
/* draw_line.c */
int draw_line(int, int, int, int);
/* draw_ramp.c */
int draw_ramp(double, double, int, int, struct Colors *, struct Categories *, struct Cell_stats *);
/* draw_text.c */
int draw_text(char *, int, int, int);
int text_bounds(char *, int, int, BOX *, int);
int text_line(int, int, int, int, int);
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
/* drwline.c */
int drwline(double, double, double, double);
/* drwscale.c */
int drwscale(double, double, double, double);
/* error.c */
int error(char *, char *, char *);
/* font.c */
char *fontfilename(char *, char *);
int check_font(char *);
int select_font(char *);
int select_standard_font(void);
int list_fonts(void);
int get_font_char(int, int *, char **, char **);
/* get_cat.c */
int get_cat(struct Categories *, struct Cell_stats *);
/* get_catcolor.c */
int get_catcolr(int, int, int);
/* getcats.c */
int getcats(void);
/* getdata.c */
int getdata(char *, char **, char **);
/* getgrid.c */
int getgrid(void);
/* getrast.c */
int getrast(void);
/* getsite.c */
int getsite(void);
/* getvect.c */
int getvect(void);
/* graph_line.c */
int graph_line(register int, register int, int, int);
/* graph_pnt.c */
int graph_point(int, int);
/* graph_text.c */
int graph_char(int *, int *, double, double, int, int);
int graph_text(int *, int *, double, double, register char *, int);
/* griddraw.c */
int drawgrid(void);
int do_newgrid_numbers(int);
void draw_tickew(double, int);
void draw_tickns(double, int);
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
/* legendtable.c */
int record_legendtable(char *, char *);
/* line_eq.c */
int line_eq(int, int, int, int, int, int);
/* line_style.c */
int set_line_style_solid(void);
int set_line_style(char *, int [9]);
int regress_line_style(void);
/* linefile.c */
int linefile(double, double, double, double);
/* lookup.c */
int lookup_from_pattern(register CELL, register int, register int);
/* main.c */
int usage(int);
/* map.c */
int map(struct Cell_head *);
/* map_gd.c */
int map(struct Cell_head *);
/* mask_grid.c */
int mask_grid(void);
/* mask_vctrs.c */
int mask_vectors(CELL *);
/* outline.c */
int outlinefile(void);
/* parse_list.c */
int parse_number_list(char *, int **);
/* patcc.c */
int main(int, char *[]);
/* patcolor.c */
int scan_patcolor(char *, int *, int *);
/* pattern.c */
int add_pattern_to_list(char *, long, int, int, int, int);
int find_pattern(char *, long *, int *, int *, int *, int *);
int read_pattern(PATTERN *, int);
int get_pattern(char *, PATTERN *);
int begin_pattern(char *);
int store_pattern(char *);
int end_pattern(void);
int any_patterns(void);
int next_pattern(PATTERN *, int);
int open_pattern_file(int, int);
int close_pattern_file(void);
int unlink_pattern_file(void);
int print_pattern(PATTERN *);
/* plfile.c */
int record_point(double, double);
int record_line(double, double, double, double);
int add_to_plfile(char *);
/* pre_legend.c */
int pre_legend(struct Categories *, struct Cell_stats *);
/* predef_pat.c */
int read_predefined_patterns(void);
/* regionfile.c */
int regionfile(char *, char *);
/* rotatebox.c */
void getbox(double, int, int);
/* scale.c */
int scale(struct Cell_head *, int, int, char *);
int unscaled(struct Cell_head *, int, int);
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
int print_session(FILE *);
/* set_color.c */
int set_color(int);
/* set_diffusion.c */
int set_diffusion_color(int);
/* set_grphcs.c */
int init_graphics(int);
int set_graphics(struct Cell_head *, int, int);
/* set_pat.c */
int set_pattern(int, char *);
int set_all_patterns(void);
/* set_width.c */
int set_width(int);
/* sitefile.c */
int sitefile(char *, char *);
/* title.c */
int title(char *);
/* trailer.c */
int trailer(struct Cell_head *, int, char *);
/* vdrawb.c */
int dvect(int, int);
/* vectdot.c */
int vect_type_is_dot(int);
/* vectdraw.c */
int vectdraw(struct Map_info *);
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
