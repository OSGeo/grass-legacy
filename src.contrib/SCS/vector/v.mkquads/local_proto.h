/* ask_for.c */
int create_name(char *, char *, char *, char *, char *);
int ask_for_name(char *, char *, char *, char *, char *);
int welcome_mat(void);
int ask_for_int(int *, char *);
int ask_for_double(double *, char *);
int ask_for_string(char *, char *);
#ifdef GRASS_GIS_H
/* convert.c */
int convert_window_to_ll(struct Cell_head *);
int print_wind(struct Cell_head *, char *);
/* wind_quads.c */
int write_window(int, double, double, double, double, struct Cell_head *);

#ifdef GRASS_VECT_H
/* init_head.c */
int init_header(FILE *, struct Cell_head *, struct dig_head *);
#endif
#endif

/* sites_quads.c */
int write_sites_line(FILE *, double, double);

#ifdef GRASS_VECT_H
/* write_quads.c */
int write_vect(FILE *, double, double, double, double, struct Map_info *, struct line_pnts *);
#endif
