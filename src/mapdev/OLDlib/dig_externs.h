char *dig_falloc ();	/* exits on error, calls _falloc () */
char *dig_frealloc ();	/* exits on error, calls _frealloc () */
char *dig_alloc_space ();	/* exits on error, calls _alloc_space () */
char *dig__falloc ();	/* returns NULL on error, calls calloc () */
char *dig__frealloc ();	/* returns NULL on error, calls calloc () */
char *dig__alloc_space (); /* returns NULL on error, calls calloc(), _frealloc() */
char *color_name ();	/* pass it an int, returns the name of the color */

double dig_point_in_area ();	/* is point inside area, return min distance to area boundary if so */
double dig_point_in_poly ();	/* is point inside polygon, return min distance to area boundary if so */
long dig__Write_line ();
long dig_Write_line ();
float dig_calc_begin_angle ();
float dig_calc_end_angle ();
char dig_new_to_old_type ();	/* convert internal type codes to/from  */
char dig_old_to_new_type ();	/* codes used in digit file		*/
double dig_distance2_point_to_line ();
double dig_xy_distance2_point_to_line ();
struct line_pnts *dig__P_get_area_xy ();
char *dig__P_init ();

double dig_unit_conversion ();

/* portable data routines  -  only to be called by library routines! */
double * dig__double_convert ();
float  * dig__float_convert ();
short  * dig__short_convert ();
long   * dig__long_convert ();
long   * dig__int_convert ();
long   * dig__plus_t_convert ();
plus_t * dig__long_convert_to_plus_t ();
plus_t * dig__long_convert_to_int ();
char   * dig__convert_buffer ();


plus_t ** dig_get_cont_lines ();
plus_t  dig_get_next_cont_line ();

struct dig_head *dig_get_head ();
struct dig_head *dig__get_head ();
struct dig_head *dig__get_cur_in_head ();
struct dig_head *dig__get_cur_out_head ();
struct dig_head *dig__get_default_in_head ();
struct dig_head *dig__get_default_out_head ();
struct dig_head *dig__get_default_port_head ();
