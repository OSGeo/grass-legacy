
/*
*  setup_ll_to_utm ()  - select the spheriod for conversions to come
*  find_quad_point ()  -  find the lowest left quad point
*  convert_window_to_ll - convert the utm window to lat, lon
*  convert_ll_to_utm()  - convert a lat/lon coordinate to utm
*  other misc functions.
*  Written by GRASS Team, Fall of 88, -mh
*/

#include	<stdio.h>
#include	"gis.h"
#include	"quad_structs.h"

#define    SPHERIOD_NUM  2	/*  clark66  */

setup_ll_to_utm (quads_info)
	struct  quads_description  *quads_info ;
{

    char *CC_spheroid_name();

	quads_info->spheroid_num = SPHERIOD_NUM ;
	quads_info->spheroid_name = CC_spheroid_name( quads_info->spheroid_num) ;

/****
	printf("\n Spheroid type being used: %s\n", quads_info->spheroid_name) ;
****/

/*  setup the conversion parameters (spheroid to be used)  */
	if (CC_u2ll_spheroid ( quads_info->spheroid_name) < 0 )
	{
		fprintf( stderr, "\n Error: Couldn't setup conversion parameters\n") ;
		exit (-1) ;
	}


	return(0) ;

}


find_quad_point (Q, W_ll, flags)
	struct  quads_description  *Q ;
	struct  Cell_head  *W_ll ;
	struct  command_flags  *flags ;
{

    int  ret ;
    int  *zone ;
    int  num_lat_quads ;	/* num of quads from equator  */
    int  num_lon_quads ;
    double south, west ;

/*  lon is east/west,  lat is north/south  */

/*  find closet quad point in the window  */
	south = W_ll->south ;
	west = W_ll->west ;

	num_lat_quads = south/QUAD_SIZE_SEC ;
	num_lon_quads = west/QUAD_SIZE_SEC ;

/*  make sure the point is in current window  */
	if (Q->north)
	{
		if (num_lat_quads * QUAD_SIZE_SEC < south)
			++num_lat_quads ;
	}
	if (Q->east)
	{
		if (num_lon_quads * QUAD_SIZE_SEC < west)
			++num_lon_quads ;
	}

/*  At this point we know for sure the lower left point is in the quad.
*   If the user wants to enclose the area we can just back up one and
*   the point will be at the next quad point outside the area.
*/
	if (flags->encompass)
	{
		num_lat_quads -= 2 ;
		num_lon_quads += 2 ;
	}

	Q->origin_lat = num_lat_quads * QUAD_SIZE_SEC ;
	Q->origin_lon = num_lon_quads * QUAD_SIZE_SEC ;


/*  convert the user given point to utm's  */
	zone = &Q->zone ;

	*zone = 0 ;
	ret = CC_ll2u ( Q->origin_lat, Q->origin_lon,
		 &Q->origin_x,  &Q->origin_y,
		 zone) ;

#ifdef DEBUG
print_quad(Q) ;
#endif

	if ( ret < 0)
	{
		fprintf( stderr, "\n Error: Couldn't convert quad point to utm's\n") ;
		exit (-1) ;
	}


	if ( *zone != W_ll->zone && *zone != W_ll->zone - 1)
	{
		fprintf( stderr, "\n Zone of the given point (zone: %d) does not match zone of the current window %d\n", *zone, W_ll->zone) ;
		fprintf(stderr, " Can't go across UTM zones.\n") ;
		exit (-1) ;
	}
	*zone = W_ll->zone;


	return(0) ;

}


convert_window_to_ll (W)
	struct  Cell_head  *W ;
{

    int  zone ;

#ifdef DEBUG
print_wind( W, " window of utm's") ;
#endif DEBUG

/*  set the zone parameter, for calcs to come  */
	zone = W->zone ;
	CC_u2ll_zone (zone) ;

/*  convert south west point of window  */
	ret_check (1,   CC_u2ll_north ( W->south)) ;
	ret_check (2,   CC_u2ll ( W->west, &W->south, &W->west) ) ;

/*  convert north east point of window  */
	ret_check (3,   CC_u2ll_north ( W->north)) ;
	ret_check (4,   CC_u2ll (W->east, &W->north, &W->east)) ;

#ifdef DEBUG
print_wind( W, " window of ll") ;
#endif DEBUG


	return(0) ;

}


convert_ll_to_utm( lon, lat, x, y, Q)
	double  lon, lat, *x, *y ;
	struct  quads_description  *Q ;
{

	CC_ll2u ( lat, lon, x, y, &Q->zone) ;

}

ret_check( funct_no, ret_value)
	int  funct_no, ret_value ;
{

	if (ret_value >= 0)
		return(0) ;

	fprintf(stderr, " ERROR: %d) Couldn't convert coordinates\n", funct_no) ;
	fprintf(stderr, " Can't go across UTM zones.\n") ;

}

/*  debugging tools follow  */

print_quad( Q) 
	struct  quads_description  *Q ;
{
    printf("\n  ------  Printing quad ---- \n") ;
    printf("  Zone: %d\n",
		 Q->zone) ;
    printf("  Spher. num: %d,  and  name '%s'\n",
		 Q->spheroid_num,   Q->spheroid_name) ;

    printf("  north: %d,  and  east %d\n",
		 Q->north,   Q->east) ;

    printf("  origin_lat: %lf,  and  origin_lon %lf\n",
		 Q->origin_lat,  Q->origin_lon) ;

    printf("  origin_x: %lf,  and  origin_y %lf\n",
		 Q->origin_x,  Q->origin_y) ;

    printf("  lat_shift: %lf,  and  lon_shift %lf\n",
		 Q->lat_shift,  Q->lon_shift) ;

}


print_wind( W, desc) 
	struct  Cell_head  *W ;
	char  *desc ;
{
    printf("\n  ------  Printing window: '%s'\n", desc) ;
    printf("  Zone: %d\n",
		 W->zone) ;

    printf("  north: %lf,  and  south %lf\n",
		 W->north,  W->south) ;

    printf("  east: %lf,  and  west %lf\n",
		 W->east,  W->west) ;

}
