
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
#include 	"projects.h"

#undef DEBUG
struct pj_info info_in, info_out;
struct Key_Value *in_proj_keys, *in_unit_keys;
struct Key_Value *out_proj_keys, *out_unit_keys;


int setup_ll_to_utm (struct quads_description *quads_info)
{
        char *ellps;
           /* get input projection parameters */
        in_proj_keys = G_get_projinfo();
        if ( in_proj_keys == NULL ){
                G_fatal_error("Error reading PROJ_INFO file.");
                exit (0);
        }
        in_unit_keys = G_get_projunits();
        if ( in_unit_keys == NULL ){
                G_fatal_error("Error reading PROJ_UNITS file.");
                exit (0);
        }
        if (pj_get_kv( &info_in, in_proj_keys, in_unit_keys) < 0) {
                G_fatal_error("Error getting in proj key values.");
                exit (0);
        }
        out_proj_keys = G_create_key_value();
        out_unit_keys = G_create_key_value();
        G_set_key_value("name", "Latitude-Longitude", out_proj_keys);
        G_set_key_value("proj", "ll", out_proj_keys);
	/* keep ellps same as input */
        ellps = G_find_key_value("ellps", in_proj_keys);
        if( ellps != NULL )
            G_set_key_value("ellps", ellps, out_proj_keys);
        else
            G_set_key_value("ellps", "wgs84", out_proj_keys);
        G_set_key_value("unit", "degree", out_unit_keys);
        G_set_key_value("units", "degrees", out_unit_keys);
        G_set_key_value("meters", "1.0", out_unit_keys);
        if (pj_get_kv( &info_out, out_proj_keys, out_unit_keys) < 0) {
                G_fatal_error("Error getting out proj key values.");
                exit (0);
        }
	quads_info->spheroid_name = out_proj_keys->value[2];

	return(0) ;

}

int find_quad_point (struct quads_description *Q,
    struct Cell_head *W_ll, struct command_flags *flags,
    int hsize, int vsize)
{

    int  ret = 0;
    int  num_lat_quads ;	/* num of quads from equator  */
    int  num_lon_quads ;
    double south, west ;


/*  lon is east/west,  lat is north/south  */

/*  find closet quad point in the window  */
	south = W_ll->south ;
	west = W_ll->west ;

	num_lat_quads = south/vsize ;
	num_lon_quads = west/hsize ;

/*  make sure the point is in current window  */
	if (Q->north)
	{
		if (num_lat_quads * vsize < south)
			++num_lat_quads ;
	}
	if (Q->east)
	{
		if (num_lon_quads * hsize < west)
			++num_lon_quads ;
	}

/*  At this point we know for sure the lower left point is in the quad.
*   If the user wants to enclose the area we can just back up one and
*   the point will be at the next quad point outside the area.
*/
	if (flags->encompass)
	{
		num_lat_quads -= 2 ;
		num_lon_quads -= 2 ;
	}

	Q->origin_lat = num_lat_quads * vsize ;
	Q->origin_lon = num_lon_quads * hsize ;


/*  convert the user given point to utm's  */

	Q->origin_y = Q->origin_lat / 3600 ;
	Q->origin_x = Q->origin_lon / 3600;

        pj_do_proj(&Q->origin_x, &Q->origin_y, &info_out, &info_in);

#ifdef DEBUG
print_quad(Q) ;
#endif

	if ( ret < 0)
	{
		fprintf( stderr, "\n Error: Couldn't convert quad point to utm's\n") ;
		exit (-1) ;
	}
	return(0) ;
}


int convert_window_to_ll (struct Cell_head *W)
{
#ifdef DEBUG
print_wind( W, " window of utm's") ;
#endif DEBUG


/*  convert south west point of window  */
        pj_do_proj(&W->west, &W->south, &info_in, &info_out);
	W->west *= 3600;
	W->south *= 3600;

/*  convert north east point of window  */
        pj_do_proj(&W->east, &W->north, &info_in, &info_out);
	W->east *= 3600;
	W->north *= 3600;

#ifdef DEBUG
print_wind( W, " window of ll") ;
#endif DEBUG
	return(0) ;
}


int convert_ll_to_utm (double lon, double lat,
	double *x, double *y, struct quads_description *Q)
{

	*x = lon / 3600;
	*y = lat / 3600;
        pj_do_proj(x, y, &info_out, &info_in);

	return 0;
}


/*  debugging tools follow  */

int print_quad (struct quads_description *Q)
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


int print_wind (struct Cell_head *W, char *desc)
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
