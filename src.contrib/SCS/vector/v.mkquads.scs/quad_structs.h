
#define	QUAD_SIZE	0.1250  /*  7 minutes, 30 seconds  in degrees*/

int LAT_LON = 0;

struct quads_description
{
    int    north ;		/*  true if north hemisphere  */
    int    east ;		/*  true if east hemisphere  */
    char   proj_cmd[256] ;	/*  projection info            */
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


