/**** externs.h ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


struct table_base * build_table ();
struct table_base * build_line_table ();
struct table_base * table_init ();
struct t_data * table_new ();
int get_area_line_pos ();
plus_t get_next_area_line ();
struct point_t *next_vert ();
struct point_t *prev_vert ();
struct poly_t * new_poly_t ();
struct array_t * Array_new_struct ();
double theta ();
