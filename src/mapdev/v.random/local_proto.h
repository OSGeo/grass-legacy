/* chk_inside.c */
double dig_x_intersect(double, double, double, double, double);
/* do_dot.c */
int do_dots(char *, char *, char *, int, int);

#ifdef GRASS_VECT_H
int tmp_find_area (struct Map_info *,P_AREA *, double *, double *,double *,
    double *,double *,double *,double *);
/* put_dots.c */
int put_dots(struct Map_info *, int, int, FILE *, int);
int make_dots(FILE *, struct Map_info *, int, double, double, double, double, int, int, double, double);
int do_point(FILE *, struct Map_info *, double, double , P_AREA *, int ,int *);
#endif
