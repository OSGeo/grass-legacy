/* @(#)check_opt.c	2.1   10/1/87 */
#include "gis.h"
#include "3d.h"
#include "options.h"

check_options()
{
    int delta_north, delta_east ;

    delta_north = from_northing - to_northing ;
    delta_east  = from_easting  - to_easting  ;

    if (abs(delta_north) > abs(delta_east))
    {
	if (delta_north > 0)
	{
	    if (delta_east > 0)
		direction = NORTH_EAST ;
	    else
		direction = NORTH_WEST ;
	}
	else
	{
	    if (delta_east > 0)
		direction = SOUTH_EAST ;
	    else
		direction = SOUTH_WEST ;
	}
    }
    else
    {
	if (delta_east > 0)
	{
	    if (delta_north > 0)
		direction = EAST_NORTH ;
	    else
		direction = EAST_SOUTH ;
	}
	else
	{
	    if (delta_north > 0)
		direction = WEST_NORTH ;
	    else
		direction = WEST_SOUTH ;
	}
    }

    printf("\n3-d view request:\n") ;
    printf("File plotted:    %s in %s\n", file, file_mapset) ;
    printf("Elevation file:  %s in %s\n", elevfile, elevfile_mapset) ;
    printf("\n") ;
    printf("            View from:    View to:\n") ;
    printf("Easting:  %12.2lf %12.2lf\n", from_easting, to_easting) ;
    printf("Northing: %12.2lf %12.2lf\n", from_northing, to_northing) ;
    printf("Height:   %12.2lf %12.2lf\n", from_height, to_height) ;
    printf("\n") ;
    printf("line frequency:         %d\n", line_freq) ;
    printf("elevation exaggeration: %4.2lf\n", exag) ;
    printf("field of view:          %4.2lf\n", field) ;

    return(0) ;
}
