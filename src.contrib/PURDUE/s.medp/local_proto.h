/* cats_att.c */
void write_cats_att(FILE *, FILE *, int, int);
/* georesid.c */
int georesid(double ***, int, int, int, double *, double *, int, int, double **, double **, double *);
/* iteration.c */
int iteration(int);
/* length.c */
double length(int, int, int, int);
/* medpolish.c */
int median_polish(double ***, double ***, int *, int, int, int, double, int);
int dblcompare(const void *, const void *);
int converged(double ***, double ***, int, int, int, double);
/* myprint.c */
int myprint(double ***, double ***);
/* readsites.c */
int readsites(FILE *, int, int);
/* report.c */
int print_report(double ***, double ***, FILE *, int, int, int, int);

#ifdef GRASS_VECT_H
/* examine.c */
int examine_grid(struct Map_info *, int *, int *, double *, double *, int, double **, double **, double *);
/* handle.c */
int handle(struct Map_info *, int, int, double, int, int, FILE *);
/* longest.c */
double longestline(struct Map_info *);
/* points.c */
void write_points(struct Map_info *, int, int);
#ifdef GRASS_GIS_H
/* init_head.c */
int init_header(FILE *, struct Cell_head *, struct dig_head *);
#endif
#endif

#ifdef GRASS_GIS_H
/* wind_overlap.c */
int G_window_overlap(struct Cell_head *, double, double, double, double);
double G_window_percentage_overlap(struct Cell_head *, double, double, double, double);
#endif
