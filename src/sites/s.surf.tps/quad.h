
/*
**  Written by H. Mitasova, I. Kosinovsky, D. Gerdes  Spring 1992
**  US Army Construction Engineering Research Lab
**  Copyright  H. Mitasova, I. Kosinovsky, D.Gerdes  USA-CERL  1992
*/

#include "data.h"
#define VOID_T char

struct quadfunc { 
    int (*compare) ();
    struct quaddata **(*divide_data) ();
    int (*add_data) ();
    int (*intersect) ();
    int (*division_check) ();
    int (*get_points) ();
};

struct quadtree {
  struct quaddata *data;
  struct quadtree *nw_leaf;
  struct quadtree *ne_leaf;
  struct quadtree *sw_leaf;
  struct quadtree *se_leaf;
  struct quadtree *parent;
  struct quadfunc *functions;
  int    quadrant;
 };

/* quad.c */
struct quadfunc *QT_functions_new(
       int (*)(struct triple *,struct quaddata *),
       struct quaddata **(*)(struct quaddata *),
       int (*)(struct triple *, struct quaddata *),
       int (*)(double, double, double, double, struct quaddata *),
       int (*)(struct quaddata *),
       int (*)(struct triple *, struct quaddata *,
               double, double, double, double, int));

struct quadtree *QT_tree_new(struct quaddata *, struct quadtree *,
       struct quadtree *, struct quadtree *, struct quadtree *,
       struct quadtree *, struct quadfunc *, int);
int QT_insert_quad(struct triple *, struct quadtree *);
int QT_divide_quad(struct quadtree *);
int QT_region_data(struct quadtree *, double, double, double, double,
       struct triple *, int);
int QT_print_tree(struct quadtree *, double, double, double, double);
/* user2.c */
int translate_quad(struct quadtree *, double, double, double);
int interp_call(struct quadtree *, struct quadtree *);
