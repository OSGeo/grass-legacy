

/*  This takes all the information we have accumulated so far and creates
*   the vector file.  Creates the quads line by line.
*   lat is north/south,  lon is east/west.
*   A quad is not a square box.  
*   Written by Grass Team, Fall of 88, -mh.
*/

#include	<stdio.h>
#include	"gis.h"
#include	"Vect.h"
#include	"quad_structs.h"
#include	"local_proto.h"

#define  WIND_DIR  "windows"

int window_quads (
    struct quads_description *Q,
    struct Cell_head *WW           /*  work window  */
)
{

	int  i, k ;
	int  window_num ;
	int  rows, cols ;
	int  num_v_rows, num_v_cols ;
	double  x,  y ;
	double  opp_x,  opp_y ;
	double  lon, lat ;
	double  lon_shift,  lat_shift ;
	double  opp_lon,  opp_lat ;

	num_v_rows = Q->num_vect_rows ;
	num_v_cols = Q->num_vect_cols ;

	rows = Q->num_rows ;
	cols = Q->num_cols ;

	lat_shift =  Q->lat_shift ;
	lon_shift =  Q->lon_shift ;


/*  write out all the vector lengths (x vectors) of the entire grid  */


	lat = Q->origin_lat ;
	for ( i = 0; i < rows; ++i)
	{
		lon = Q->origin_lon ;
		for (k = 0; k < cols; ++k)
		{
			opp_lon = lon + lon_shift ;
			opp_lat = lat + lat_shift ;

		    /*  lower left point of window  */
			convert_ll_to_utm( lon, lat, &x, &y, Q) ;
		    /*  opposite point of window  */
			convert_ll_to_utm( opp_lon, opp_lat, &opp_x, &opp_y, Q) ;
			window_num = i * cols + k + 1 ;

			write_window( window_num, x, y, opp_x, opp_y, WW ) ;

			lon = opp_lon ;
		}

		lat += lat_shift ;
	}

	return (0) ;
}



int write_window (
    int window_num,
    double x1,
    double y1,
    double x2,
    double y2,
    struct Cell_head *WW           /*  work window  */
)
{

	int  t ;
	double  east, west ;
	double  north, south ;
	double  ew_res, ns_res ;
	char  name[156] ;

/*  WW contains current window, save those values  */

	east = WW->east ;
	west = WW->west ;
	north = WW->north ;
	south = WW->south ;

	ew_res = WW->ew_res ;
	ns_res = WW->ns_res ;

	WW->east = (x1 > x2) ? x1 : x2 ;
	WW->west = (x1 < x2) ? x1 : x2 ;
	WW->north = (y1 > y2) ? y1 : y2 ;
	WW->south = (y1 < y2) ? y1 : y2 ;

/*  set the window depending on the window resolution  */

	t =  (north - WW->north) /ns_res ;
	WW->north = north - t * ns_res ;

	t =  (WW->south - south) /ns_res ;
	WW->south = south + t * ns_res ;

	t =  (east - WW->east) /ew_res ;
	WW->east = east - t * ew_res ;

	t =  (WW->west - west) /ew_res ;
	WW->west = west + t * ew_res ;

	sprintf( name, "quad.%d", window_num) ;
	G__put_window( WW, WIND_DIR, name) ;

	return 0;
}

