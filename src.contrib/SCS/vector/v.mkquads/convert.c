
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
#include	"CC.h"
#include	"quad_structs.h"

#define    SPHERIOD_NUM  2	/*  clark66  */
static int ret_check (int , int );

int 
setup_ll_to_utm (struct quads_description *quads_info)
{
	quads_info->spheroid_num = SPHERIOD_NUM ;
	quads_info->spheroid_name = CC_spheroid_name( quads_info->spheroid_num) ;

/****
	fprintf (stdout,"\n Spheroid type being used: %s\n", quads_info->spheroid_name) ;
****/

/*  setup the conversion parameters (spheroid to be used)  */
	if (CC_u2ll_spheroid ( quads_info->spheroid_name) < 0 )
	{
		fprintf( stderr, "\n Error: Couldn't setup conversion parameters\n") ;
		exit (-1) ;
	}


	return(0) ;

}


int 
find_quad_point (struct quads_description *Q, struct Cell_head *W_ll, struct command_flags *flags)
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


int 
convert_window_to_ll (struct Cell_head *W)
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


int 
convert_ll_to_utm (double lon, double lat, double *x, double *y, struct quads_description *Q)
{

	CC_ll2u ( lat, lon, x, y, &Q->zone) ;
	return 0;
}

static int ret_check (int funct_no, int ret_value)
{

	if (ret_value >= 0)
		return(0) ;

	fprintf(stderr, " ERROR: %d) Couldn't convert coordinates\n", funct_no) ;
	fprintf(stderr, " Can't go across UTM zones.\n") ;
	return 0;
}

/*  debugging tools follow  */

int 
print_quad (struct quads_description *Q)
{
    fprintf (stdout,"\n  ------  Printing quad ---- \n") ;
    fprintf (stdout,"  Zone: %d\n",
		 Q->zone) ;
    fprintf (stdout,"  Spher. num: %d,  and  name '%s'\n",
		 Q->spheroid_num,   Q->spheroid_name) ;

    fprintf (stdout,"  north: %d,  and  east %d\n",
		 Q->north,   Q->east) ;

    fprintf (stdout,"  origin_lat: %f,  and  origin_lon %f\n",
		 Q->origin_lat,  Q->origin_lon) ;

    fprintf (stdout,"  origin_x: %f,  and  origin_y %f\n",
		 Q->origin_x,  Q->origin_y) ;

    fprintf (stdout,"  lat_shift: %f,  and  lon_shift %f\n",
		 Q->lat_shift,  Q->lon_shift) ;
	return 0;
}


int 
print_wind (struct Cell_head *W, char *desc)
{
    fprintf (stdout,"\n  ------  Printing window: '%s'\n", desc) ;
    fprintf (stdout,"  Zone: %d\n",
		 W->zone) ;

    fprintf (stdout,"  north: %f,  and  south %f\n",
		 W->north,  W->south) ;

    fprintf (stdout,"  east: %f,  and  west %f\n",
		 W->east,  W->west) ;
	return 0;
}
