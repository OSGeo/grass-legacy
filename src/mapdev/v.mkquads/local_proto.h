/* ask_for.c */
int create_name(char *, char *, char *, char *, char *);
int ask_for_name(char *, char *, char *, char *, char *);
int welcome_mat(void);
int ask_for_int(int *, char *);
int ask_for_double(double *, char *);
int ask_for_string(char *, char *);
/* calc_quads.c */
int setup_ll_to_utm(struct quads_description *);
int convert_ll_to_utm(double, double, double *, double *, struct quads_description *);
int print_quad(struct quads_description *);
/* init_quad.c */
int check_args(int, char *[], char *, struct command_flags *);
int dashed_args(char *, struct command_flags *);
/* reg_quads.c */
int reg_quads(FILE *, struct quads_description *);
/* report_quads.c */
int report_quads(FILE *, struct quads_description *, struct command_flags *);
/* sites_quads.c */
int sites_quads(FILE *, struct quads_description *);

#ifdef GRASS_GIS_H
/* calc_quads.c */
int calculate_quads(struct quads_description *, struct Cell_head *, struct command_flags *, int, int);
/* convert.c */
int find_quad_point(struct quads_description *, struct Cell_head *, struct command_flags *, int, int);
int convert_window_to_ll(struct Cell_head *);
int print_wind(struct Cell_head *, char *);
/* init_quad.c */
int init_quad_struct(struct quads_description *, struct Cell_head *, int, int);
/* wind_quads.c */
int window_quads(struct quads_description *, struct Cell_head *);
int write_window(int, double, double, double, double, struct Cell_head *);
#endif

#ifdef GRASS_VECT_H
/* init_head.c */
int init_header(FILE *, struct Cell_head *, struct dig_head *);
/* write_quads.c */
int write_quads(FILE *, struct quads_description *, struct Map_info *);
int write_vect(FILE *, double, double, double, double, struct Map_info *, struct line_pnts *);
#endif
