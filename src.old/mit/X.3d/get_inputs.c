/* %W%  %G% */
#include "options.h"
#include "gis.h"

get_inputs(do_it, do_erase)
	int *do_it, *do_erase ;
{
	static int here_already = 0 ;
	char go[2] ;
	char erase[2] ;
	char *mapset ;

	if(! here_already)
	{
	/* Set some defaults */
		to_easting =  (window.east  + window.west ) / 2 ;
		to_northing = (window.north + window.south) / 2 ;
		to_height = 0 ;
		from_easting = window.west - (window.east - window.west) ;
		from_northing = window.south - (window.north - window.south) ;
		from_height = 5000 ;
		exag = 2.0 ;
		line_freq = 10 ;
		field = 20.0 ;

		strcpy (file, "elevation") ;
		strcpy (elevfile, "elevation") ;
		here_already = 1 ;
	}
	strcpy(go, "Y") ;
	strcpy(erase, "Y") ;

	V_clear() ;
V_line ( 1,"  ---------------------------------------------------------------------") ;
V_line ( 2,"    You will be viewing the area bounded by the current window:") ;
V_line ( 5,"    SOUTH:                 NORTH:              resolution:") ;
V_line ( 6,"    WEST:                  EAST:") ;
V_line ( 9,"    RUN? Y/N:        ERASE? Y/N:") ;
V_line (10,"  ---------------------------------------------------------------------") ;
V_line (11,"         Please identify your desired viewing coordinates:") ;
V_line (13,"    Viewing from:                    Center of area to be viewed:") ;
V_line (14,"        Northing                         Northing") ;
V_line (15,"        Easting                          Easting") ;
V_line (16,"        Elevation                        Elevation") ;
V_line (18,"    Vertical Exageration             File used for height:") ;
V_line (19,"    Field of View (degrees)          Map used for color:");
V_line (20,"    Line frequency") ;
V_line (21,"  ---------------------------------------------------------------------") ;

	V_const( &(window.south),   'd',  5, 11, 10) ;
	V_const( &(window.north),   'd',  5, 34, 10) ;
	V_const( &(window.west ),   'd',  6, 11, 10) ;
	V_const( &(window.east ),   'd',  6, 34, 10) ;
	V_ques ( go             ,   's',  9, 14,  2) ;
	V_ques ( erase          ,   's',  9, 33,  2) ;
	V_ques ( &(window.ns_res ), 'd',  5, 60, 10) ;
	V_ques ( &from_northing ,   'd', 14, 18, 10) ;
	V_ques ( &from_easting  ,   'd', 15, 18, 10) ;
	V_ques ( &from_height   ,   'd', 16, 18, 10) ;
	V_ques ( &to_northing   ,   'd', 14, 51, 10) ;
	V_ques ( &to_easting    ,   'd', 15, 51, 10) ;
	V_ques ( &to_height     ,   'd', 16, 51, 10) ;
	V_ques ( elevfile       ,   's', 18, 59, 14) ;
	V_ques ( file           ,   's', 19, 59, 14) ;
	V_ques ( &exag          ,   'd', 18, 28,  7) ;
	V_ques ( &field         ,   'd', 19, 28,  7) ;
	V_ques ( &line_freq     ,   'i', 20, 28,  5) ;

	V_call() ;
	window.ew_res = window.ns_res ;

/* Identify mapsets for these files */
	mapset = G_find_cell (file, "") ;
	strcpy(file_mapset, mapset) ;

	mapset = G_find_cell (elevfile, "") ;
	strcpy(elevfile_mapset, mapset) ;

	if (erase[0] == 'Y' || erase[0] == 'y')
		*do_erase = 1 ;
	else
		*do_erase = 0 ;

	if (go[0] == 'Y' || go[0] == 'y')
		*do_it = 1 ;
	else
		*do_it = 0 ;

	return(0) ;
}
