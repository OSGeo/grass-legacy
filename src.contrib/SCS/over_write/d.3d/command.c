#include "options.h"
#include "gis.h"

set_default_options()
{
	char *strcpy() ;
	strcpy (file, "elevation") ;
	strcpy (file_mapset, "PERMANENT") ;
	strcpy (elevfile, "elevation") ;
	strcpy (elevfile_mapset, "PERMANENT") ;
	to_easting =  (window.east  + window.west ) / 2 ;
	to_northing = (window.north + window.south) / 2 ;
	to_height = 0 ;
	from_easting = window.west - (window.east - window.west) ;
	from_northing = window.south - (window.north - window.south) ;
	from_height = 30000. ;
	exag = 2.0 ;
	line_freq = 10 ;
	field = 20.0 ;
	lines_only = 0 ;
	do_zero = 0 ;
/*-->   line_color = -1 ;    changed  RL Glenn, SCS, 3/12/91 */
	line_color = D_translate_color("black") ;
}
