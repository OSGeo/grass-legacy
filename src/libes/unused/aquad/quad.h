
#define	QUAD_SIZE	7.30  /*  7 degrees, 30 seconds  */
#define	QUAD_SIZE_SEC	450.00  /*  all in seconds  */

#define    SPHERIOD_NUM  2	/*  clark66  */

struct quads_description
{
    int    zone ;		/*  UTM zone  */
    int    north ;		/*  true if north hemisphere  */
    int    east ;		/*  true if east hemisphere   */
    int    spheroid_num ;	/*  index to spheroid array   */
    char   *spheroid_name ;	/*  spheroid name  */
    double origin_lat ;         /*  lower left point of grid   */
    double origin_lon ;
    double lat_shift ;		/* positive or negative quad size */
    double lon_shift ;		/* which direction we go  */
};


