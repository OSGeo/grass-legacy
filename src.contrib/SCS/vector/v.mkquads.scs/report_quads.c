

/*  This takes all the information we have accumulated so far and creates
*   the vector file.  Creates the quads line by line.
*   lat is north/south,  lon is east/west.
*   A quad is not a square box.  
*   Written by Grass Team, Fall of 88, -mh.
*/

#include	<stdio.h>
#include	"quad_structs.h"
#include	"dig_defines.h"

report_quads (fp_report, Q, flags)
	FILE  *fp_report ;
	struct  quads_description  *Q ;
	struct  command_flags  *flags ;
{

	int  i, k ;
	int  num_v_rows, num_v_cols ;
	double  x,  y ;
	double  lon, lat ;
	double  lon_shift,  lat_shift ;

	num_v_rows = Q->num_vect_rows ;
	num_v_cols = Q->num_vect_cols ;


	lat_shift =  Q->lat_shift ;
	lon_shift =  Q->lon_shift ;


	fprintf( fp_report, "\n%30s\n", "QUAD POINTS") ;
	fprintf( fp_report, "\n         Projection Used: %s\n\n", Q->proj_cmd) ;
	fprintf( fp_report, "         Lon             Lat               Easting       Northing\n") ;
	fprintf( fp_report, "         ---             ---               -------       --------\n") ;

/*  write out all the vector lengths (x vectors) of the entire grid  */


	lat = Q->origin_lat ;
	for ( i = 0; i < num_v_rows; ++i)
	{
		lon = Q->origin_lon ;
		for (k = 0; k < num_v_cols; ++k)
		{
			convert_ll_to_proj( lon, lat, &x, &y, Q) ;
			write_report_line( fp_report, k+1, lon, lat, x, y) ;
			lon += lon_shift ;
		}
		fprintf( fp_report, "\n") ;

		lat += lat_shift ;
	}

	if(flags->encompass)
	{
	fprintf( fp_report, "\n These quad points encompass the area.\n") ;
	fprintf( fp_report, "\n Widen the window to see the results.\n") ;
	}

	return (0) ;

}


static  write_report_line( fp_report, cnt, lon, lat, x, y )
	FILE  *fp_report ;
	int  cnt ;
	double  lon, lat, x, y ;
{

	fprintf( fp_report, " %d)   %3.3lf  %3.3lf    %10.2lf    %10.2lf\n",
		cnt, lon, lat, x, y) ;
}

