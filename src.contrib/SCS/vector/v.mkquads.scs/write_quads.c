

/*  This takes all the information we have accumulated so far and creates
*   the vector file.  Creates the quads line by line.
*   lat is north/south,  lon is east/west.
*   A quad is not a square box.  
*   Written by Grass Team, Fall of 88, -mh.
*/

#include	<stdio.h>
#include	"quad_structs.h"
#include	"Vect.h"

write_quads (fp_digit, Q, Map)
	FILE  *fp_digit ;
	struct  quads_description  *Q ;
	struct Map_info *Map;
{

	int  i, k ;
	int  rows, cols ;
	int  num_v_rows, num_v_cols ;
	double  x,  y ;
	double  x2,  y2 ;
	double  next_x,  next_y ;
	double  lon, lat ;
	double  lon_shift,  lat_shift ;
	double  next_lon,  next_lat ;
	char  buffer[128] ;

    struct line_pnts *Points;

	Points = Vect_new_line_struct();

	num_v_rows = Q->num_vect_rows ;
	num_v_cols = Q->num_vect_cols ;

	rows = Q->num_rows ;
	cols = Q->num_cols ;

	lat_shift =  Q->lat_shift ;
	lon_shift =  Q->lon_shift ;

/*  write out all the vector lengths (x vectors) of the entire grid  */

/******
	printf("\n Writing out vector rows...") ;
******/

	lat = Q->origin_lat ;
	for ( i = 0; i < num_v_rows; ++i)
	{
		lon = Q->origin_lon ;
		for (k = 0; k < cols; ++k)
		{
			next_lon = lon + lon_shift ;

			convert_ll_to_proj( lon, lat, &x, &y, Q) ;
			convert_ll_to_proj( next_lon, lat, &next_x, &y2, Q) ;
			write_vect( fp_digit, x, y, next_x, y2, Map, Points ) ;

			lon = next_lon ;
		}

		lat += lat_shift ;
	}



/*  write out all the vector widths (y vectors) of the entire grid  */

/******
	printf("\n Writing out vector columns...") ;
******/

	lon = Q->origin_lon ;
	for ( k = 0; k < num_v_cols; ++k)
	{
		lat = Q->origin_lat ;
		for (i = 0; i < rows; ++i)
		{
			next_lat = lat + lat_shift ;

			convert_ll_to_proj( lon, lat, &x, &y, Q) ;
			convert_ll_to_proj( lon, next_lat, &x2, &next_y, Q) ;
			write_vect( fp_digit, x, y, x2, next_y, Map, Points ) ;

			lat = next_lat ;
		}

		lon += lon_shift ;
	}

	Vect_destroy_line_struct (Points);

	return (0) ;


}


static  double  xarray[10] ; 
static  double  yarray[10] ; 

#define  NUM_POINTS  2

write_vect( fp_digit, x1, y1, x2, y2, Map, Points )
	FILE  *fp_digit ;
	double  x1, y1, x2, y2 ;
	struct Map_info *Map;
	struct line_pnts *Points;
{

	xarray[0] = x1 ;
	xarray[1] = x2 ;
	yarray[0] = y1 ;
	yarray[1] = y2 ;
/*	fprintf(stderr,"X %lf y %lf x %lf y %lf\n",x1,y1,x2,y2);*/

	/*dig_Write_line( fp_digit, AREA, xarray, yarray, NUM_POINTS) ; */

	if (0 > Vect_copy_xy_to_pnts (Points, xarray, yarray, NUM_POINTS))
		G_fatal_error ("Out of memory");

	Vect_write_line (Map, AREA, Points);


}


