
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

struct triple  *point_new ();
struct quaddata *data_new ();
int quad_compare ();
int quad_add_data ();
int quad_intersect ();
int quad_division_check();
struct quaddata ** quad_divide_data();
int quad_get_points();
