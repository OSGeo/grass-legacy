
/*
**  Written by H. Mitasova, I. Kosinovsky, D. Gerdes  Spring 1992
**  US Army Construction Engineering Research Lab
**  Copyright  H. Mitasova, I. Kosinovsky, D.Gerdes  USA-CERL  1992
*/

#define VOID_T char

struct quadfunc { 
    int (*compare) ();
    VOID_T **(*divide_data) ();
    int (*add_data) ();
    int (*intersect) ();
    int (*division_check) ();
    int (*get_points) ();
};

struct quadtree {
  VOID_T *data;
  struct quadtree *nw_leaf;
  struct quadtree *ne_leaf;
  struct quadtree *sw_leaf;
  struct quadtree *se_leaf;
  struct quadtree *parent;
  struct quadfunc *functions;
  int    quadrant;
 };

struct quadfunc * QT_functions_new(); 
struct quadtree * QT_tree_new ();
int QT_insert_quad ();
int QT_region_data ();
int QT_print_tree();
