void *dig_alloc_space(int,int *,int,void *,int);	/* exits on error, calls _alloc_space () */
void *dig__alloc_space(int,int *,int,void *,int); /* returns NULL on error, calls calloc(), _frealloc() */
void *dig_falloc (int ,int);	/* exits on error, calls _falloc () */
void *dig_frealloc( void *, int ,int, int );	/* exits on error, calls _frealloc () */
void *dig__falloc(int ,int );	/* returns NULL on error, calls calloc () */
void *dig__frealloc( void *, int ,int, int );	/* returns NULL on error, calls calloc () */
char *color_name (int);	/* pass it an int, returns the name of the color */

float dig_calc_begin_angle ( struct line_pnts *, double );
float dig_calc_end_angle( struct line_pnts *, double );
char dig_new_to_old_type (char);   /* convert internal type codes to/from  */
char dig_old_to_new_type (char);   /* codes used in digit file		*/
char *dig_float_point ( char *, int , double );
double dig_point_in_area(struct Map_info *, double ,double , P_AREA *);
double dig_x_intersect ( double , double , double , double , double );
double dig_point_in_poly ( double ,double , struct line_pnts *);
double dig_distance2_point_to_line ( double ,double , double ,double ,double ,double );
double dig_xy_distance2_point_to_line ( double *,double *, double ,double ,double ,double );
double dig_xy_distance3_point_to_line ( double *,double *, double ,double ,double ,double , int *);

double dig_unit_conversion (void);

/* portable data routines  -  only to be called by library routines! */
double *dig__double_convert ( double *,double *, int , struct dig_head *);
float *dig__float_convert ( float *,float *, int , struct dig_head *);
short *dig__short_convert ( short *in,short *out, int , struct dig_head *);
long *dig__long_convert ( long *,long *, int , struct dig_head *);
long *dig__int_convert ( int *, long *, int , struct dig_head *);
long *dig__plus_t_convert ( plus_t *, long *, int , struct dig_head *);
int *dig__long_convert_to_int ( long *, int *, int , struct dig_head *);
plus_t *dig__long_convert_to_plus_t ( long *, plus_t *, int , struct dig_head *);
char *dig__convert_buffer (int);

plus_t **dig_get_cont_lines ( struct Map_info *, plus_t , double , int );
plus_t dig_get_next_cont_line ( struct Map_info *, plus_t , double , int );

struct dig_head *dig__get_cur_in_head (void);
struct dig_head *dig__get_cur_out_head (void);
struct dig_head *dig_get_head (void);
struct dig_head *dig__get_head (void);
struct dig_head *dig__get_default_in_head (void);
struct dig_head *dig__get_default_out_head (void);
struct dig_head *dig__get_default_port_head (void);
int dig__Init_portable_code (int );
int dig__fill_head_portable (struct dig_head *);
int dig__set_cur_in_head (struct dig_head *);
int dig__set_cur_out_head (struct dig_head *);
int dig__fread_port_D ( double *, int, FILE *);
int dig__fread_port_F ( float *, int, FILE *);
int dig__fread_port_L ( long *, int, FILE *);
int dig__fread_port_S ( short *, int, FILE *);
int dig__fread_port_I ( int *, int, FILE *);
int dig__fread_port_P ( plus_t *, int, FILE *);
int dig__fread_port_C ( char *, int, FILE *);
int dig__fwrite_port_D ( double *, int, FILE *);
int dig__fwrite_port_F ( float *, int, FILE *);
int dig__fwrite_port_L ( long *, int, FILE *);
int dig__fwrite_port_S ( short *, int, FILE *);
int dig__fwrite_port_I ( int *, int, FILE *);
int dig__fwrite_port_P ( plus_t *, int , FILE *);
int dig__fwrite_port_C ( char *, int , FILE *);


/******************************************************************************/
/* proto.h */
int dig_build_area_with_line ( struct Map_info *, int , P_AREA *);
int dig_new_area (struct Map_info *, P_AREA *,int);
int dig__del_area ( struct Map_info *, int );
int dig_del_area (struct Map_info *, int area);
int dig_angle_next_line ( struct Map_info *, int , int );
int dig_area_bound_box ( struct Map_info *, P_AREA *);
int dig__del_att(struct Map_info *, int);
int dig_del_att(struct Map_info *, int);
int dig__new_att(struct Map_info *, double,double, char, int, int, long);
int dig_new_att(struct Map_info *, double,double,char, int, int);
int dig_update_att(struct Map_info *, int);
int dig_bound_box2(struct line_pnts *,double *,double *,double *,double *,long );
int dig_is_line_degenerate ( struct line_pnts *, double);
int dig_check_nodes ( struct Map_info *, struct new_node *,struct line_pnts *);
int dig_which_node ( struct Map_info *, double  *,double *, double );
int dig_in_area_bbox ( P_AREA *, double ,double );
int dig_start_clock (long *);
int dig_stop_clock ( long *);
char *dig_stop_clock_str ( long *);
int dig_write_file_checks ( FILE *, struct Map_info *, struct Plus_head *);
int dig_do_file_checks(struct Map_info *, char *,char *,char *);
int dig_find_area(struct Map_info *,P_AREA *,double *,double *,double *,double);
int dig_find_area2 ( struct Map_info *, P_AREA *, double *);
int dig_find_area2_poly ( struct line_pnts *, double *);
int dig_new_isle ( struct Map_info *, P_AREA *, plus_t );
int dig_del_isle ( struct Map_info *, int );
int dig_set_distance_to_line_tolerance ( double );
int dig_test_for_intersection ( double , double , double , double , double , double , double , double );
int dig_find_intersection ( double , double , double , double , double , double , double , double , double *, double *);
int dig_load_plus ( struct Map_info *, FILE *, int );
int dig_map_to_head ( struct Map_info *, struct Plus_head *);
int dig_head_to_map ( struct Plus_head *, struct Map_info *);
int dig_Mem_load_file ( FILE *, char **);
int dig_Mem_release_file (void);
int dig_mread ( char *, unsigned int , unsigned int , int );
int dig_mwrite ( char *, unsigned int , unsigned int , int );
int dig_mseek ( int , long , int );
int dig_snap_line_to_node ( struct Map_info *, int ,int , struct line_pnts *);
int dig_node_del_line ( P_NODE *, int line);
int dig_node_add_line(struct Map_info *,P_NODE *,int,struct line_pnts *,int);
int dig_add_line_to_node ( int ,int , char , struct Map_info *, struct line_pnts *);
char dig_old_to_new_type (char );
char dig_new_to_old_type ( char);
int dig_point_to_area ( struct Map_info *, double ,double );
int dig_point_to_next_area ( struct Map_info *, double ,double , double *);
int dig_point_to_line ( struct Map_info *, double ,double , char );
int dig_in_line_bbox( P_LINE *, double ,double );
int dig_check_dist ( struct Map_info *, int , double, double, double *);
int dig__check_dist ( struct Map_info *, struct line_pnts *, double , double , double *);
int dig_center_check( P_LINE *, int ,int , double ,double );
int dig_point_by_line ( struct Map_info *, double ,double , double ,double , char );
int dig_by_line_bbox ( P_LINE *, double ,double , double ,double );
int dig_prune ( struct line_pnts *, double);
int dig_write_head_ascii( FILE *, struct dig_head *);
int dig_read_head_ascii( FILE *, struct dig_head *);
int dig_node_alloc_line ( P_NODE *, int add);
int dig_alloc_node ( struct Map_info *, int );
int dig_alloc_line ( struct Map_info *, int );
int dig_alloc_area ( struct Map_info *, int );
int dig_alloc_isle ( struct Map_info *, int );
int dig_alloc_points ( struct line_pnts *, int );
int dig_area_alloc_line ( P_AREA *, int );
int dig_area_alloc_isle ( P_AREA *, int );
int dig_isle_alloc_line ( P_ISLE *, int );
int dig_alloc_att ( struct Map_info *, int );
int dig_out_of_memory (void);
int dig_struct_copy (void *,void *, int);
int dig_rmcr (char *);
int dig_write_plus_file(FILE * , struct Map_info * , struct Plus_head * );
int dig_write_nodes( FILE * , struct Map_info * , struct Plus_head * );
int dig_write_lines( FILE * , struct Map_info * , struct Plus_head * );
int dig_write_areas( FILE * , struct Map_info * , struct Plus_head * );
int dig_write_atts( FILE * , struct Map_info * , struct Plus_head * );
int dig_write_isles( FILE * , struct Map_info * , struct Plus_head * );
int dig_x_Rd_P_node ( struct Map_info *, struct P_node *, FILE *);
int dig_x_Wr_P_node ( struct Map_info *, struct P_node *, FILE *);
int dig_x_Rd_P_line ( struct Map_info *, struct P_line *, FILE *);
int dig_x_Wr_P_line ( struct Map_info *, struct P_line *, FILE *);
int dig_x_Rd_P_area ( struct Map_info *, struct P_area *, FILE *);
int dig_x_Wr_P_area ( struct Map_info *, struct P_area *, FILE *);
int dig_x_Rd_P_isle ( struct Map_info *, struct P_isle *, FILE *);
int dig_x_Wr_P_isle ( struct Map_info *, struct P_isle *, FILE *);
int dig_x_Rd_P_att ( struct Map_info *, struct P_att *, FILE *);
int dig_x_Wr_P_att ( struct Map_info *, struct P_att *, FILE *);
int dig_x_Rd_Plus_head ( struct Map_info *, struct Plus_head *, FILE *);
int dig_x_Wr_Plus_head ( struct Map_info *, struct Plus_head *, FILE *);
