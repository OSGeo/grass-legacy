
/*
**  Written by H. Mitasova, I. Kosinovsky, D. Gerdes  Spring 1992
**  US Army Construction Engineering Research Lab
**  Copyright  H. Mitasova, I. Kosinovsky, D.Gerdes  USA-CERL  1992
*/

#define NW   1
#define NE   2
#define SW   3
#define SE   4

struct triple   {
  double x;
  double y;
  double z;
};

struct quaddata {
  double x_orig;
  double y_orig;
  int    n_rows;
  int    n_cols;
  int    n_points;
  struct triple  *points;
};

/* data.c */
struct triple *point_new(double, double, double);
struct quaddata *data_new(double, double, int, int, int);
int quad_compare(struct triple *, struct quaddata *);
int quad_add_data(struct triple *, struct quaddata *);
int quad_intersect(double, double, double, double, struct quaddata *);
int quad_division_check(struct quaddata *);
struct quaddata **quad_divide_data(struct quaddata *);
int quad_get_points(struct triple *, struct quaddata *, double, double, double, double, int);
/* user3.c */
int COGRR1(double, double, int, int, int, struct triple *);
int POINT(int, struct triple *);
