/* dataquad.c */
struct triple *quad_point_new(double, double, double, double);
struct quaddata *quad_data_new(double, double, double, double, int, int, int, int);
int quad_compare(struct triple *, struct quaddata *);
int quad_add_data(struct triple *, struct quaddata *, double);
int quad_intersect(struct quaddata *, struct quaddata *);
int quad_division_check(struct quaddata *, int);
struct quaddata **quad_divide_data(struct quaddata *, int, double);
int quad_get_points(struct quaddata *, struct quaddata *, int);
