#include <string.h>
#include "options.h"
#include "gis.h"

int set_default_options (void)
{
	to_easting =  (window.east  + window.west ) / 2 ;
	to_northing = (window.north + window.south) / 2 ;
	from_easting = window.west - (window.east - window.west) ;
	from_northing = window.south - (window.north - window.south) ;
	exag = 2.0 ;
	strcpy (file, "elevation") ;
	strcpy (file_mapset, "PERMANENT") ;
	strcpy (elevfile, "elevation") ;
	strcpy (elevfile_mapset, "PERMANENT") ;
	exag = 2.0 ;
	line_freq = 1 ;
	field = 30.0 ;
	lines_only = 1 ;
	do_null = 0 ;
	line_color = -1 ;

	return 0;
}
