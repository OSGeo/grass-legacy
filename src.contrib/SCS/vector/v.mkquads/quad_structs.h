
#define	QUAD_SIZE	7.30  /*  7 degrees, 30 seconds  */
#define	QUAD_SIZE_SEC	450.00  /*  all in seconds  */


struct quads_description
{
    int    zone ;		/*  UTM zone */
    int    north ;		/*  true if north hemisphere  */
    int    east ;		/*  true if east hemisphere  */
    int    spheroid_num ;	/*  index to spheroid array  */
    char   *spheroid_name ;	/*  spheroid name            */
    double origin_lat ;        /*  lower left point of grid  */
    double origin_lon ;
    double origin_x ;        /*  lower left point of grid  */
    double origin_y ;		/*  in UTM's  */
    double lat_shift ;		/* positive or negative quad size */
    double lon_shift ;		/* which direction we go  */
    int    num_rows ;
    int    num_cols ;
    int    num_vect_rows ;
    int    num_vect_cols ;
};



/*  flags that get set from the command line  */
struct command_flags
{
    int    vectors ;  	/* create the vector file of quads  */
    int    sites ;	/*  site file of quad points  */
    int    windows ;	/*  window files  from quad points  */
    int    encompass ;	/*  encompass map */
    int    reg ;	/*  registration file for `digit`  */
    int    files ;
    int    usage ;
};


