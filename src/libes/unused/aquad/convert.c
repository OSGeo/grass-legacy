
/*
*  setup_ll_to_utm ()  - select the spheriod for conversions to come
*  find_quad_point ()  -  find the lowest left quad point
*  convert_ll_to_utm()  - convert a lat/lon coordinate to utm
*  print_quad_error()  -  print a decent error message for bad given point.
*  other misc functions.
*  Written by the GRASS Team, Summer of 89, -mh
*/

#include	<stdio.h>
#include	"quad.h"


setup_ll_to_utm (quads_info)
	struct  quads_description  *quads_info ;
{

    char *CC_spheroid_name();

	quads_info->spheroid_num = SPHERIOD_NUM ;
	quads_info->spheroid_name = CC_spheroid_name( quads_info->spheroid_num) ;

/*  setup the conversion parameters (spheroid to be used)  */
	if (CC_u2ll_spheroid ( quads_info->spheroid_name) < 0 )
	{
		fprintf( stderr, "\n Error: Couldn't setup conversion parameters\n") ;
		exit (-1) ;
	}


	return(0) ;

}   /*  setup_ll_to_utm  */


find_quad_point (Q)
	struct  quads_description  *Q ;
{

    int  ret ;
    int  *zone ;
    int  num_lat_quads ;	/* num of quads from equator  */
    int  num_lon_quads ;
    double south, west ;

/*  lon is east/west,  lat is north/south  */

/*  find closet quad point in the window  */
	south = Q->origin_lat ;
	west = Q->origin_lon ;

	num_lat_quads = south/QUAD_SIZE_SEC ;
	num_lon_quads = west/QUAD_SIZE_SEC ;

	ret = 0 ;

/*  make sure the given point is a true quad point  */
	if ( (num_lat_quads * QUAD_SIZE_SEC) != south)
		ret = -1 ;

	if ( (num_lon_quads * QUAD_SIZE_SEC) != west)
		ret = -1 ;

	Q->origin_lat = num_lat_quads * QUAD_SIZE_SEC ;
	Q->origin_lon = num_lon_quads * QUAD_SIZE_SEC ;

	return(ret) ;

}


print_quad_error (Q)
	struct  quads_description  *Q ;
{

	double  x, y ;
	double  lat, lon ;


	fprintf (stdout," The point given on the command line wasn't a TRUE quad corner point\n") ;
	fprintf (stdout," A possible quad corner point:\n") ;

	lat = Q->origin_lat ;
	lon = Q->origin_lon ;

	convert_ll_to_utm( lon, lat, &x, &y, Q) ;
	write_report_line( lon, lat, x, y) ;

}


convert_ll_to_utm( lon, lat, x, y, Q)
	double  lon, lat, *x, *y ;
	struct  quads_description  *Q ;
{

/*  if the zone is not known, set to 0  */

	Q->zone = 0 ;
	CC_ll2u ( lat, lon, x, y, &Q->zone) ;

}


/*  debugging tools follow  */

print_info_struct( Q) 
	struct  quads_description  *Q ;
{
    fprintf (stdout,"\n  ------  Printing quad ---- \n") ;
    fprintf (stdout,"  Zone: %d\n",
		 Q->zone) ;
    fprintf (stdout,"  Spher. num: %d,  and  name '%s'\n",
		 Q->spheroid_num,   Q->spheroid_name) ;

    fprintf (stdout,"  north: %d,  and  east %d\n",
		 Q->north,   Q->east) ;

    fprintf (stdout,"  origin_lat: %lf,  and  origin_lon %lf\n",
		 Q->origin_lat,  Q->origin_lon) ;

    fprintf (stdout,"  lat_shift: %lf,  and  lon_shift %lf\n",
		 Q->lat_shift,  Q->lon_shift) ;

}

