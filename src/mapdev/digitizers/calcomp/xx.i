# 1 "coll_pts.c" 








# 1 "/grass.src/4.0/src/mapdev/Vlib/Vect.h" 1
# 1 "/grass.src/4.0/src/mapdev/diglib/digit.h" 1



# 1 "/usr/include/stdio.h" 1





extern	struct	_iobuf {
	int	_cnt;
	unsigned char *_ptr;
	unsigned char *_base;
	int	_bufsiz;
	short	_flag;
	char	_file;		
} _iob[];



















# 36 "/usr/include/stdio.h" 

















extern struct _iobuf	*fopen();
extern struct _iobuf	*fdopen();
extern struct _iobuf	*freopen();
extern struct _iobuf	*popen();
extern struct _iobuf	*tmpfile();
extern long	ftell();
extern char	*fgets();
extern char	*gets();
extern char	*sprintf();
extern char	*ctermid();
extern char	*cuserid();
extern char	*tempnam();
extern char	*tmpnam();






# 5 "/grass.src/4.0/src/mapdev/diglib/digit.h" 2
# 1 "/grass.src/4.0/src/mapdev/diglib/dig_head.h" 1






# 1 "/grass.src/4.0/src/mapdev/diglib/portable.h" 1













# 8 "/grass.src/4.0/src/mapdev/diglib/dig_head.h" 2














struct dig_head
{
	    
    char organization[30] ;
    char date[20] ;
    char your_name[20] ;
    char map_name[41] ;
    char source_date[11] ;
    long  orig_scale ;
    char line_3[73] ;
    int plani_zone ;
    double W, E, S, N ;
    double digit_thresh ;
    double map_thresh ;

	    
	    
	    
    int Version_Major;
    int Version_Minor;
    int Back_Major;
    int Back_Minor;

    
    int portable;
    unsigned char dbl_cnvrt[8];
    unsigned char flt_cnvrt[4];
    unsigned char lng_cnvrt[4];
    unsigned char shrt_cnvrt[2];
    int dbl_quick;
    int flt_quick;
    int lng_quick;
    int shrt_quick;

    struct Map_info *Map;	
};





# 6 "/grass.src/4.0/src/mapdev/diglib/digit.h" 2
# 1 "/grass.src/4.0/src/mapdev/diglib/dig_defines.h" 1







































































# 96 "/grass.src/4.0/src/mapdev/diglib/dig_defines.h" 

# 7 "/grass.src/4.0/src/mapdev/diglib/digit.h" 2
# 1 "/grass.src/4.0/src/mapdev/diglib/dig_macros.h" 1



























# 8 "/grass.src/4.0/src/mapdev/diglib/digit.h" 2
# 1 "/grass.src/4.0/src/mapdev/diglib/dig_structs.h" 1







# 1 "/grass.src/4.0/src/mapdev/diglib/dig_head.h" 1


# 62 "/grass.src/4.0/src/mapdev/diglib/dig_head.h" 

# 9 "/grass.src/4.0/src/mapdev/diglib/dig_structs.h" 2















typedef int plus_t;








struct Map_info
{
    short id;			
    struct P_node *Node;		  
    struct P_area *Area;		  
    struct P_line *Line;		
    struct P_att  *Att;
    struct P_isle *Isle;

    plus_t n_nodes;		
    plus_t n_lines;		
    plus_t n_areas;		
    plus_t n_atts;			
    plus_t n_isles;
    plus_t n_alines;		
    plus_t n_llines;		
    plus_t n_plines;		
    int    n_points;		

    plus_t alloc_nodes;		
    plus_t alloc_lines;		
    plus_t alloc_areas;		
    plus_t alloc_atts;		
    plus_t alloc_isles;		

    







    struct _iobuf *dig_fp;		
    struct _iobuf *att_fp;		
    char *digit_file;		
    char *att_file;			

    char *plus_file;			
    char *coor_file;			

    
    struct dig_head head;

    double snap_thresh;
    double prune_thresh;

    int all_areas;	
    int all_isles;	


    
    



    int open;   
		
		
		
    int mode;   
    int level;	
    plus_t next_line;   

    char *name;			
    char *mapset;

    
    int Constraint_region_flag;
    int Constraint_type_flag;
    double Constraint_N;
    double Constraint_S;
    double Constraint_E;
    double Constraint_W;
    int Constraint_type;
};


struct Plus_head
{
    int Major;		
    int Minor;

    plus_t n_nodes;
    plus_t n_lines;
    plus_t n_areas;
    plus_t n_atts;
    plus_t n_isles;
    plus_t n_llines;
    plus_t n_alines;
    plus_t n_plines;
    int    n_points;

    long Node_offset;
    long Line_offset;
    long Area_offset;
    long Att_offset;
    long Isle_offset;

    long Dig_size;	
    long Att_size;	
    long Dig_code;	
    long Att_code;

    int all_areas;	
    int all_isles;	

    double snap_thresh;
    double prune_thresh;

    long  Back_Major;	
    long  Back_Minor;
    long future3;
    long future4;
    double F1, F2, F3, F4;	

    char Dig_name[50];
    char filler[50];
};

struct P_line
{
    plus_t N1;		
    plus_t N2;		
		
    plus_t left;	
    plus_t right;	

    double N;		
    double S;
    double E;
    double W;

    long  offset;	
    plus_t   att;		
    char  type;		
};

struct P_node
{
    double  x;		
    double  y;		
    plus_t  alloc_lines;
    plus_t  n_lines;	
			
    plus_t  *lines; 	
    float  *angles;	
    char   alive;	
};

struct P_area
{
    double  N;		
    double  S;
    double  E;
    double  W;
    plus_t   att;	
    plus_t  n_lines;	
    plus_t  alloc_lines;

    plus_t  n_isles;	
    plus_t  alloc_isles;
    plus_t  *lines;	
    plus_t  *isles;	
    char   alive;	
};


struct P_isle
{
    double  N;		
    double  S;
    double  E;
    double  W;
    plus_t  area;	
    plus_t  n_lines;	
    plus_t  alloc_lines;
    plus_t  *lines;	
    char   alive;	
};

struct P_att
{
    double x;		
    double y;		
    long   offset;	
    int    cat;		
    plus_t  index;	
    char   type;	
};

struct new_node {
    plus_t N1;
    plus_t N2;
    int    cnt;
};

struct line_pnts {
    double *x;
    double *y;
    int     n_points;
    int     alloc_points;
};


# 9 "/grass.src/4.0/src/mapdev/diglib/digit.h" 2
# 1 "/grass.src/4.0/src/mapdev/diglib/dig_externs.h" 1

char *dig_falloc ();	
char *dig_frealloc ();	
char *dig_alloc_space ();	
char *dig__falloc ();	
char *dig__frealloc ();	
char *dig__alloc_space (); 
char *color_name ();	

double dig_point_in_area ();	
double dig_point_in_poly ();	
float dig_calc_begin_angle ();
float dig_calc_end_angle ();
char dig_new_to_old_type ();	
char dig_old_to_new_type ();	
double dig_distance2_point_to_line ();
double dig_xy_distance2_point_to_line ();
char * dig_float_point ();


double dig_unit_conversion ();


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



char *Vect__P_init ();
struct dig_head *Vect__get_default_in_head ();
struct dig_head *Vect__get_default_out_head ();
struct dig_head *Vect__get_default_port_head ();
struct line_pnts * Vect_new_line_struct ();
struct line_pnts * Vect__new_line_struct ();
char * Vect__P_init_new_plus ();
char * Vect__P_init();
long Vect_write_line ();
long Vect_x__Write_line ();
# 10 "/grass.src/4.0/src/mapdev/diglib/digit.h" 2
# 1 "/grass.src/4.0/src/mapdev/diglib/dig_globs.h" 1
# 3 "/grass.src/4.0/src/mapdev/diglib/dig_globs.h" 




extern int Lines_In_Memory;
extern char *Mem_Line_Ptr;
extern char *Mem_curr_position;

# 11 "/grass.src/4.0/src/mapdev/diglib/digit.h" 2


# 2 "/grass.src/4.0/src/mapdev/Vlib/Vect.h" 2
# 10 "coll_pts.c" 2










collect_points(mode, type, n_points, xarr, yarr)
	int mode, type ;
	int *n_points ;
	double **xarr, **yarr ;
{
	int     Xraw;
	int     Yraw;
	int		Xlast ;
	int		Ylast ;
	int		KeyHit ;
	int		last_key ;
	int		loop ;
	int stream_mode ;
	int run_mode ;
	int save_mode ;
	double	*xptr ;
	double	*yptr ;
	char message[128] ;
	char *dig_falloc() ;
	char *dig_frealloc() ;
	static int alloc_points = 0 ;
	static double *xarray, *yarray ;

	if (! alloc_points)
	{
		alloc_points     = 1000 ;
		xarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;
		yarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;
	}

	if (type == 0x04)
	{
	    run_mode = 0 ;
	    save_mode = mode;
	}
	else
	    run_mode = mode ;

	xptr = xarray ;
	yptr = yarray ;

	Clear_info() ;


	delay(5) ;
	D_flush() ;

	Write_info(1, " # Points       Easting     Northing") ;

	if (run_mode == 1)
	{

		while ( D_readall (&Xraw, &Yraw) < 0)
			D_ask_if_err() ;
		transform_a_into_b ((double) Xraw, (double) Yraw, xptr, yptr) ;
		Xlast = Xraw ;
		Ylast = Yraw ;

		*n_points = 1 ;
		stream_mode = 1 ;
		sprintf(message, "   %6d   %12.2lf %12.2lf", *n_points, *xptr, *yptr);
		Write_info(2, message) ;
		xptr++ ;
		yptr++ ;
		Write_info(4, " STREAM mode ") ;
	}
	else
	{
		Xlast = 0.0 ;
		Ylast = 0.0 ;
		*n_points = 0 ;
		stream_mode = 0 ;
		sprintf(message, "   %6d   %12.2lf %12.2lf", *n_points, 0.0, 0.0);
		Write_info(2, message) ;
		Write_info(4, " POINT mode ") ;
	}

	loop = 1 ;
	last_key = -1 ;


	while (loop)
	{

		while ( (KeyHit = D_readall (&Xraw, &Yraw)) < 0)
			D_ask_if_err() ;


	





		if (KeyHit == last_key)
			continue ;
		else
			last_key = -1 ;


	
		if (KeyHit)
		{

			switch (KeyHit)
			 {
				case 2:
					if (type == 0x04)
					    return (save_mode);
					loop = 0 ;
					Write_info(3, " processing..") ;
					continue ;
					break ;

				case 4:
					if (type == 0x04)
					{
					    last_key = KeyHit;
					    break;
					}
					if (run_mode == 0)
					{
						stream_mode = 1 ;
						run_mode = 1 ;
						Write_info(4, " STREAM mode ") ;
					}
					else
					{
						run_mode = 0 ;
						Write_info(4, " POINT mode ") ;
					}

					last_key = KeyHit ;
					break ;

				case 3:
					Write_info(2, "    Updating  MONITOR") ;
					plot_points( type, *n_points, xarray, yarray,
						CLR_HIGHLIGHT, 0);
					Write_info(2, "    finished: continue  digitizing...") ;
					last_key = KeyHit ;
					continue ;
					break ;

				default:
					last_key = KeyHit ;
					break ;

			 }		
		}

		if (run_mode == 0  && KeyHit != 1 )
			continue ;


	    
		if ( Xlast == Xraw  &&  Ylast == Yraw)
			continue ;
		
		Xlast = Xraw ;
		Ylast = Yraw ;

		
		if (*n_points + 2 == alloc_points)
		{
			int old;

			old = alloc_points;
			alloc_points += 100 ;
			xarray = (double *)dig_frealloc((char *)xarray, alloc_points, sizeof(double), old);
			yarray = (double *)dig_frealloc((char *)yarray, alloc_points, sizeof(double), old);
			xptr = xarray + *n_points ;
			yptr = yarray + *n_points ;
		}

	    
		transform_a_into_b ((double) Xraw, (double) Yraw, xptr, yptr) ;
		(*n_points)++ ;

		if (type == 0x04)
		{
		    loop = 0;
		    xarray[1] = xarray[0];
		    yarray[1] = yarray[0];
		    (*n_points)++;
		}

	    
		if (run_mode == 0)
		{
		

			if (*n_points == 1)
				plot_points( type, 1, xptr, yptr,
				    CLR_HIGHLIGHT, CLR_HIGHLIGHT);
			else
				plot_points( type, *n_points, xarray, yarray,
				    CLR_HIGHLIGHT, 0);
		}
				

	    
		sprintf(message, "   %6d   %12.2lf %12.2lf", *n_points, *xptr, *yptr);
		Write_info(2, message) ;

	    
		xptr++ ;
		yptr++ ;

	}


    
    plot_points( type, 1, xarray, yarray, CLR_ERASE, CLR_ERASE);
    plot_points( type, *n_points, xarray, yarray, CLR_ERASE, CLR_ERASE);


    
	*xarr = xarray ;
	*yarr = yarray ;

	if (type == 0x04)
	    return (save_mode);

	return(stream_mode) ;

}	


