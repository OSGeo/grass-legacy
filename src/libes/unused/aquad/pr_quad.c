

#include	<stdio.h>
#include	"quad.h"

print_quad (Q)
	struct  quads_description  *Q ;
{

	double  x,  y ;
	double  lon_origin, lat_origin ;
	double  lon_opp,  lat_opp ;


	lat_origin = Q->origin_lat ;
	lon_origin = Q->origin_lon ;

/*  Calculate the opposite corner  */
	lat_opp = lat_origin + Q->lat_shift ;
	lon_opp = lon_origin + Q->lon_shift ;

/*  Origin Point -  Lower Left  */
	convert_ll_to_utm( lon_origin, lat_origin, &x, &y, Q) ;
	write_report_line( lon_origin, lat_origin, x, y) ;

/*  Upper Left Point  */
	convert_ll_to_utm( lon_origin, lat_opp, &x, &y, Q) ;
	write_report_line( lon_origin, lat_opp, x, y) ;

/*  Upper Right Point  */
	convert_ll_to_utm( lon_opp, lat_opp, &x, &y, Q) ;
	write_report_line( lon_opp, lat_opp, x, y) ;

/*  Lower Right Point  */
	convert_ll_to_utm( lon_opp, lat_origin, &x, &y, Q) ;
	write_report_line( lon_opp, lat_origin, x, y) ;

	return (0) ;

}


write_report_line( lon, lat, x, y )
	double  lon, lat, x, y ;
{
	char  lon_buf[80] ;
	char  lat_buf[80] ;

	CC_lon_format ( lon, lon_buf) ;
	CC_lat_format ( lat, lat_buf) ;

	fprintf (stdout," %-15s  %-15s    %10.2lf    %10.2lf\n",
		lon_buf, lat_buf, x, y) ;
}

