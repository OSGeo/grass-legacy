#ifndef GRASS_VECT_H
#define GRASS_VECT_H
#include "vect/digit.h"

/*   ANSI prototypes for the libes/vect32/Vlib functions */
int Vect_close (struct Map_info *);
int V1_close_nat (struct Map_info *);
int V1_close_shp (struct Map_info *);
int V1_close_post (struct Map_info *);
int V2_close_nat (struct Map_info *);
int V2_close_shp (struct Map_info *);
int V2_close_post (struct Map_info *);
int Vect_set_constraint_region (struct Map_info *, double, double, double, double);
int Vect_set_constraint_type (struct Map_info *, int);
int Vect_remove_constraints (struct Map_info *);
int Vect_get_node_point (struct Map_info *, int, double *, double *);
int Vect_get_area_points (struct Map_info *, int, struct line_pnts *);
int Vect_get_area_centroid ( struct Map_info *, int );
int Vect_get_area_num_isles ( struct Map_info *, int );
int Vect_get_area_isle ( struct Map_info *, int, int );
int Vect_get_isle_points (struct Map_info *, int, struct line_pnts *);
int Vect_find_line (struct Map_info *, double, double, int, double);
int Vect_find_area (struct Map_info *, double, double);
int Vect_point_in_area (struct Map_info *, int, double, double);
int Vect_tin_get_z (struct Map_info *, double, double, double *, double *, double *);


int Vect_print_header (struct Map_info *);
int Vect__init_head (struct dig_head *);
int Vect_copy_head_data (struct dig_head *, struct dig_head *);
int Vect_level (struct Map_info *);
int V2_num_nodes (struct Map_info *);
int V2_num_lines (struct Map_info *);
int V2_num_areas (struct Map_info *);
int V2_get_area_bbox (struct Map_info *, int, double *, double *, double *, double *);
int V2_get_line_bbox (struct Map_info *, int, double *, double *, double *, double *);
struct line_pnts *Vect_new_line_struct (void);
struct line_cats *Vect_new_cats_struct (void);
struct cat_list *Vect_new_cat_list (void);
struct ilist *Vect_new_list (void);
struct line_pnts *Vect__new_line_struct (void);
struct line_cats *Vect__new_cats_struct (void);
int Vect_destroy_line_struct (struct line_pnts *);
int Vect_destroy_cats_struct (struct line_cats *);
int Vect_destroy_cat_list (struct cat_list *);
int Vect_destroy_list (struct ilist *);
int Vect_copy_xy_to_pnts (struct line_pnts *, double *, double *, int);
int Vect_copy_xyz_to_pnts (struct line_pnts *, double *, double *, double *, int);
int Vect_reset_line (struct line_pnts *);
int Vect_append_point (struct line_pnts *, double, double);
int Vect_append_3d_point (struct line_pnts *, double, double, double);
int Vect_append_points (struct line_pnts *, struct line_pnts *, int direction);
int Vect_cat_set (struct line_cats *, int, int);
int Vect_cat_get (struct line_cats *, int, int *);
int Vect_cat_del (struct line_cats *, int);
int Vect_reset_cats (struct line_cats *);
int Vect_reset_list (struct ilist *);
int Vect_str_to_cat_list (char *, struct cat_list *);
int Vect_array_to_cat_list (int *, int, struct cat_list *);
int Vect_cat_in_cat_list (int, struct cat_list *);
struct field_info *Vect_get_field_info (char *, char *, int);
int Vect_cat_in_array (int, int *, int);
int Vect_copy_pnts_to_xy (struct line_pnts *, double *, double *, int *);
int Vect_copy_pnts_to_xyz (struct line_pnts *, double *, double *, double *, int *);
int Vect_type_to_code (int);
int Vect_code_to_type (int);
int Vect_type_to_store (int);
int Vect_type_from_store (int);
int Vect_set_open_level (int);
int Vect_open_old (struct Map_info *, char *, char *);
int Vect_open_new (struct Map_info *, char *, int);
/*int Vect__open_update_1 (struct Map_info *, char *);*/
int V1_open_old_nat (struct Map_info *);
int V1_open_old_shp (struct Map_info *);
int V1_open_old_post (struct Map_info *);
int V1_open_new_nat (struct Map_info *, char *, int);
int V1_open_new_shp (struct Map_info *, char *, int);
int V1_open_new_post (struct Map_info *, char *, int);
/*int V1__open_update_1 (struct Map_info *, char *); */
int V2_open_old_nat (struct Map_info *);
int V2_open_old_shp (struct Map_info *);
/*
int V2__open_new_1 (struct Map_info *, char *, int);
int V2_open_update (struct Map_info *, char *);
int V2_open_new_plus (struct Map_info *, char *);
int V2__init_for_create_plus (struct Map_info *, char *);
int V2__open_update_1 (struct Map_info *, char *);
int V2__setup_for_digit (struct Map_info *, char *); 
*/
int Vect_open_topo (struct Map_info *);
int Vect_save_topo ( struct Map_info *);
    
int Vect_coor_info ( struct Map_info *, struct Coor_info *);

int Vect_get_point_in_area (struct Map_info *, int, double *, double *);
int Vect__intersect_line_with_poly (struct line_pnts *, double, struct line_pnts *);
int Vect_get_point_in_poly (struct line_pnts *, double *, double *);
int Vect_find_poly_centroid (struct line_pnts *, double *, double *);
int Vect_point_in_islands (struct Map_info *, int, double, double);
int Vect_get_point_in_poly_isl (struct line_pnts *, struct line_pnts **, int, double *, double *);
int Vect_init (void);
/*
int Vect__write_head_binary (struct Map_info *, struct dig_head *);
int Vect__read_head_binary (struct Map_info *, struct dig_head *);
*/
int Vect__write_head (struct Map_info *);
int Vect__read_head (struct Map_info *);

int dig_Rd_P_node (struct Plus_head *, int i, FILE *);
int dig_Wr_P_node (struct Plus_head *, int i, FILE *);
int dig_Rd_P_line (struct Plus_head *, int i, FILE *);
int dig_Wr_P_line (struct Plus_head *, int i, FILE *);
int dig_Rd_P_area (struct Plus_head *, int i, FILE *);
int dig_Wr_P_area (struct Plus_head *, int i, FILE *);
int dig_Rd_P_isle (struct Plus_head *, int i, FILE *);
int dig_Wr_P_isle (struct Plus_head *, int i, FILE *);
int dig_Rd_Plus_head (FILE *, struct Plus_head *);
int dig_Wr_Plus_head (FILE *, struct Plus_head *);

int V1_read_line (struct Map_info *, struct line_pnts *, struct line_cats *, long);
int V1_read_line_nat (struct Map_info *, struct line_pnts *, struct line_cats *, long);
int V1_read_line_shp (struct Map_info *, struct line_pnts *, struct line_cats *, long);
int V1_read_line_post (struct Map_info *, struct line_pnts *, struct line_cats *, long);
int Vect_read_next_line (struct Map_info *, struct line_pnts *, struct line_cats *);
int V1_read_next_line (struct Map_info *, struct line_pnts *, struct line_cats *);
int V1_read_next_line_nat (struct Map_info *, struct line_pnts *, struct line_cats *);
int V1_read_next_line_shp (struct Map_info *, struct line_pnts *, struct line_cats *);
int V1_read_next_line_post (struct Map_info *, struct line_pnts *, struct line_cats *);
int V2_read_line (struct Map_info *, struct line_pnts *, struct line_cats *, int);
int V2_read_line_nat (struct Map_info *, struct line_pnts *, struct line_cats *, int);
int V2_read_line_shp (struct Map_info *, struct line_pnts *, struct line_cats *, int);
int V2_read_line_post (struct Map_info *, struct line_pnts *, struct line_cats *, int);
int V2_read_next_line (struct Map_info *, struct line_pnts *, struct line_cats *);
int V2_read_next_line_nat (struct Map_info *, struct line_pnts *, struct line_cats *);
int V2_read_next_line_shp (struct Map_info *, struct line_pnts *, struct line_cats *);
int V2_read_next_line_post (struct Map_info *, struct line_pnts *, struct line_cats *);

int Vect_line_alive ( struct Map_info *, int);

long Vect_next_line_offset (struct Map_info *);
long Vect_next_line_offset_nat (struct Map_info *);
long Vect_next_line_offset_shp (struct Map_info *);
long Vect_next_line_offset_post (struct Map_info *);

int V__map_overlap (struct Map_info *, double, double, double, double);
int Vect_rewind (struct Map_info *);
int V1_rewind_nat (struct Map_info *);
int V1_rewind_shp (struct Map_info *);
int V1_rewind_post (struct Map_info *);
int V2_rewind_nat (struct Map_info *);
int V2_rewind_shp (struct Map_info *);
int V2_rewind_post (struct Map_info *);
/*
int Vect__get_window (struct Map_info *, double *, double *, double *, double *);
*/
long Vect_write_line (struct Map_info *, int type, struct line_pnts *, struct line_cats *);
long V1_write_line_nat (struct Map_info *, int type, struct line_pnts *, struct line_cats *);
long V1_write_line_shp (struct Map_info *, int type, struct line_pnts *, struct line_cats *);
long V1_write_line_post (struct Map_info *, int type, struct line_pnts *, struct line_cats *);
long V1_rewrite_line (struct Map_info *, long offset, int type, struct line_pnts *, struct line_cats *);
long V1_rewrite_line_nat (struct Map_info *, long offset, int type, struct line_pnts *, struct line_cats *);
long V1_rewrite_line_shp (struct Map_info *, long offset, int type, struct line_pnts *, struct line_cats *);
long V1_rewrite_line_post (struct Map_info *, long offset, int type, struct line_pnts *, struct line_cats *);

int V1_delete_line (struct Map_info *, long offset);
int V1_delete_line_nat (struct Map_info *, long offset);

int Vect_build ( struct Map_info *Map, FILE *msgout );
int Vect_build_nat ( struct Map_info *Map, FILE *msgout );
int Vect_build_shp ( struct Map_info *Map, FILE *msgout );
int Vect_build_post ( struct Map_info *Map, FILE *msgout );
int Vect_topo_dump ( struct Plus_head *plus, FILE *out );

int Vect_point_on_line ( struct line_pnts *, double, 
	             double *, double *, double *, double *, double *);
double Vect_line_length ( struct line_pnts *);
int Vect_line_distance ( struct line_pnts *, double, double, double *, double *, double *, double *, double *);

int Vect_point_in_box (double, double, double, BOUND_BOX *);
int Vect_box_overlap (BOUND_BOX *, BOUND_BOX *);
int Vect_box_copy (BOUND_BOX *, BOUND_BOX *);
int Vect_box_extend (BOUND_BOX *, BOUND_BOX *);
int Vect_get_line_box (struct Map_info *, int, BOUND_BOX *);
int Vect_get_area_box (struct Map_info *, int, BOUND_BOX *);
int Vect_get_isle_box (struct Map_info *, int, BOUND_BOX *);
int Vect_get_map_box (struct Map_info *, BOUND_BOX *);

int Vect_select_lines_by_box (struct Map_info *, BOUND_BOX *, int, struct ilist *);
int Vect_select_areas_by_box (struct Map_info *, BOUND_BOX *, struct ilist *);
int Vect_select_isles_by_box (struct Map_info *, BOUND_BOX *, struct ilist *);

#endif

