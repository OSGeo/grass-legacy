/**** externs.h ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992

  Added prototypes - Bill Hughes, 1999/06/19
*/
/* alloc.c */
int alloc_poly_t(struct poly_t *, int, int);
int alloc_poly_t_lines(struct sub_poly *, int);
int alloc_intersections(struct intersects *, int);
/* area.c */
int get_area_line_pos_subpoly ( struct Map_info *, int , int , plus_t);
int get_area_line_pos ( struct Map_info *, int , plus_t);
plus_t get_next_area_line ( struct Map_info *, plus_t , plus_t);
plus_t get_prev_area_line ( struct Map_info *, plus_t , plus_t);
/* array.c */
int Array_set_chunk_size(int);
struct array_t *Array_new_struct(int);
int Array_alloc(struct array_t *, int);
int Array_destroy(struct array_t *);
/* bbox.c */
int line_bboxes_cross (P_LINE *,P_LINE *);
int area_bboxes_cross (P_AREA *,P_AREA *);
/* build.c */
int build_polys(struct Map_info [2], struct Map_info *, struct t_data *);
int swap_pointers(void **, void **);
int find_next_intersection(struct array_p *, struct poly_vert_info *,
    struct poly_vert_info *, int, int, struct poly_t *);
int dump_active_list(struct array_p *);
int load_active_list (struct t_data *,struct array_p *,plus_t,int);
int draw_vertex(double, double, char *);
int vert2_after_vert1(struct poly_vert_info *, struct poly_vert_info *,
    struct poly_t *[2], int);
int dump_line(struct line_pnts *);
/* dump.c */
int dump_table(struct t_data *);
int dump_table_entry(struct t_data *);
/* init.c */
struct table_base *table_init(void);
struct t_data *table_new(struct table_base *);
int table_cleanup(struct table_base *);
int cutter_init(struct Map_info *);
/* interior.c */
int interior_polys(struct Map_info [2], struct Map_info *, struct t_data *);
int inter_clean(int);
int update_intersect_table(struct Map_info [2]);
int dump_nointersect(struct Map_info [2]);
/* interline.c */
int interior_lines(struct Map_info [2], struct Map_info *, struct t_data *);
/* intersect.c */
int intersect(struct line_t, struct line_t);
int ccw(struct point_t, struct point_t, struct point_t);
int point_right_of_line(struct point_t, struct point_t, struct point_t);
int tribbles_intersect(struct tribble, struct tribble);
/* label.c */
int write_out_new_poly_att(struct Map_info [2], struct Map_info *, struct poly_t *[2], struct line_pnts *);
int poly_t_load_islands(struct poly_t *, struct poly_t *[2]);
int poly_t_interior_load_islands(struct poly_t *, struct poly_t *[2]);
int poly_t_load_base_points ( struct poly_t *, struct line_pnts *);
int dump_opoly(struct poly_t *);
int dump_opoly0(struct poly_t *);
int write_out_new_line_att(struct Map_info *, struct line_pnts *, int, int);
/* lbuild.c */
int build_lines(struct Map_info [2], struct Map_info *, struct t_data *);
int load_active_line_list (struct t_data *, struct array_p *, plus_t, int);
int find_next_line_intersection(struct array_p *, struct poly_vert_info *, struct poly_vert_info *, int, int, struct t_data *);
/* linecent.c */
int get_line_center(double *, double *, struct line_pnts *);
/* linecros.c */
int test_for_intersection(double, double, double, double, double, double, double, double);
int find_intersection(double, double, double, double, double, double, double, double, double *, double *);
/* linepnts.c */
struct point_t *next_vert (struct Map_info *, struct line_pnts *,
    plus_t, int, plus_t, int);
struct point_t *prev_vert ( struct Map_info *, struct line_pnts *,
    plus_t , int    , plus_t , int);
int seg_colinear (struct point_t *, struct point_t *,
     struct point_t *, struct point_t *);
/* lsort.c */
int sort_intersections_on_line(struct array_p *, int);
/* ltable.c */
struct table_base *build_line_table(struct Map_info *, struct Map_info *);
int intersect_line_table (struct Map_info *, struct Map_info *,
    struct line_pnts *, struct line_pnts *, plus_t , plus_t ,
    struct table_base *);
/* main.c */
int debugf(char *, ...);
/* mod.c */
int ring_mod(int, int);
/* plane.c */
int point_to_right_of_segment(struct line_t *, struct point_t *);
/* point.c */
int Cut_get_point_in_poly_t(struct poly_t *, double *, double *);
int Cut__intersect_line_with_poly(struct line_pnts *, double, struct intersects *, int);
int Cut_find_poly_centroid(struct line_pnts *, double *, double *);
int Cut_point_in_islands(struct poly_t *, double, double);
int intersect_append_point(struct intersects *, double, int);
/* poly.c */
struct poly_t *new_poly_t(void);
int load_poly (struct Map_info *, plus_t , struct poly_t *);
int bump_vert_to_next_line ( struct sub_poly *, struct poly_vert_info *);
int poly_next_vert(struct poly_t *, struct poly_vert_info *, struct poly_vert_info *);
int poly_prev_vert(struct poly_t *, struct poly_vert_info *, struct poly_vert_info *);
/* readline.c */
int V2_read_line_poly_order (struct Map_info *,
    struct line_pnts *, int, plus_t);
/* sort.c */
int sort_intersections_on_poly ( struct array_p *, struct poly_t *, int);
/* table.c */
struct table_base *build_table(struct Map_info *, struct Map_info *);
int do_vertex_intersection(void);
int intersect_table (struct Map_info *, struct Map_info *,
    struct line_pnts *,struct line_pnts *,plus_t,plus_t,struct table_base *);
int do_colinear_intersection(void);
int destroy_table(struct table_base *);
int draw_tribble(struct tribble *, char *);
/* theta.c */
double theta(struct point_t, struct point_t);
/* tools.c */
int seg_bboxes_cross(struct line_t *, struct line_t *);
int vertices_touch(struct line_t *, struct line_t *);
/* vertex.c */
int inter_to_poly_vert (struct t_data *, struct poly_t *, int,
    struct poly_vert_info *);
int node_to_poly_vert ( struct poly_t *, struct poly_vert_info *,
    struct poly_vert_info *);
int add_point ( struct poly_vert_info *, struct line_pnts *,
    struct line_pnts *, int);
int add_points_till_eol ( struct poly_t *, struct poly_vert_info *,
    struct line_pnts *, struct line_pnts *);
int add_points_till_inter ( struct poly_t *, struct poly_vert_info *,
    struct poly_vert_info *, struct line_pnts *, struct line_pnts *);
int sub_poly_line_start_pos ( struct sub_poly *, int line);
int dump_poly ( struct poly_t *);
int dump_vert ( struct poly_vert_info *);
int inter_to_line_vert ( struct t_data *, int, struct poly_vert_info *);
int add_line_points_till_eol (struct poly_vert_info *, struct line_pnts *,
    struct line_pnts *, int);
int add_line_points_till_inter (struct poly_vert_info *,
    struct poly_vert_info *, struct line_pnts *, struct line_pnts *, int);
/* write.c */
int write_cur_line (struct Map_info *, struct Map_info *,
    struct line_pnts *, int, plus_t);
/* xlines.c */
int lines_intersect(long, long, long, long, long, long, long, long, long *, long *);
