/* barscale.c */
int record_barscale(char *, char *);
/* case.c */
int lowercase(register char *);
int uppercase(register char *);
/* cat_color.c */
int cat_color_num(register int);
/* cellfile.c */
int cellfile(char *, char *);
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
/* diamond.c */
int diamond(int, int);
/* distance.c */
double distance(double, double);
/* do_barscale.c */
int do_barscales(void);
/* do_grid.c */
int do_grid(void);
int do_grid_numbers(int);
/* do_labels.c */
int do_labels(int);
/* do_legend.c */
int do_legends(void);
/* do_legendfin.c */
int do_legends(void);
/* do_newgrid.c */
int do_newgrid(void);
/* do_plfile.c */
int do_plfile(int);
/* do_region.c */
int do_region(int);
/* do_rscale.c */
int do_rscale(void);
/* do_sites.c */
int do_sites(void);
/* do_vectors.c */
int do_vectors(int);
/* error.c */
int error(char *, char *, char *);
/* getcats.c */
int getcats(void);
/* getd.c */
int getd(char *, char **, char **);
/* getdata.c */
int getdata(char *, char **, char **);
/* getgrid.c */
int getgrid(void);
/* getnline.c */
int getdata(char *, char **, char **);
/* getpat.c */
int getpat(char *, char **, char **);
/* getrast.c */
int getrast(void);
/* getsite.c */
int getsite(void);
/* getvect.c */
int getvect(void);
/* header.c */
int header(int, int, char *);
/* input.c */
int input(int, char *, char *[]);
int gobble_input(void);
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
/* main.c */
int usage(int);
/* mask_grid.c */
int pre_mask_grid(void);
int mask_grid(void);
/* outline.c */
int outlinefile(void);
/* plfile.c */
int record_point(double, double);
int record_line(double, double, double, double);
int add_to_plfile(char *);
/* redraw_bar.c */
int redraw_barscale(double, double, int, double, int, int);
/* regionfile.c */
int regionfile(char *, char *);
/* rotatebox.c */
void getbox(double, int, int);
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
/* set_diffus.c */
int set_diffusion_color(int);
/* set_grphcs.c */
int init_graphics(int);
/* set_width.c */
int set_width(int);
/* sitefile.c */
int sitefile(char *, char *);
/* title.c */
int title(char *);
/* vdrawb.c */
int dvect(int, int);
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

#ifdef GRASS_VECT_H
/* vectdraw.c */
int vectdraw(struct Map_info *);
#endif

#ifdef GRASS_GIS_H
int print_session(FILE *);
/* do_labels.c */
int do_label(FILE *, int);
/* do_barscale.c */
int do_barscale(FILE *);
/* ctable.c */
int ctable(struct Categories *, struct Colors *, struct Cell_stats *);
/* do_cat.c */
int do_cats(struct Colors *);
int do_cat(FILE *, struct Colors *);
/* do_ramp.c */
int do_ramps(struct Cell_stats *);
int do_ramp(FILE *, struct Cell_stats);
/* get_cat.c */
int get_cat(struct Cell_stats *);
/* get_catcolor.c */
int get_catcolr(DCELL,int,int);
/* lookup.c */
int lookup_from_pattern(register CELL, register int, register int);
/* map10.c */
int map(struct Cell_head *);
/* mask_vctrs.c */
int mask_vectors(CELL *);
/* parse_list.c */
int parse_val_list (char *, DCELL **);
/* pre_legend.c */
int pre_legend(struct Cell_stats *);
/* scale.c */
int scale(struct Cell_head *, int, int, char *);
int unscaled(struct Cell_head *, int, int);
/* set_grphcs.c */
int set_graphics(struct Cell_head *, int, int);
/* trailer.c */
int trailer(struct Cell_head *, int, char *);
#endif
