/* %W%  %G% */
#include "options.h"
#include "gis.h"

get_inputs(do_it, erase_color)
int *do_it, *erase_color ;
{
	static int here_already = 0 ;
	static char go[2] ;
	static char erase[16] ;
	static char linesonly[2] ;
	static char dozero[2] ;
	static char linecolor[16] ;
	char *mapset ;

	if(! here_already)
	{
		/* Set some defaults */
		to_easting =  (window.east  + window.west ) / 2 ;
		to_northing = (window.north + window.south) / 2 ;
		to_height = 0 ;
		from_easting = window.west - (window.east - window.west) ;
		from_northing = window.south - (window.north - window.south) ;
		from_height = 30000 ;
		exag = 2.0 ;
		line_freq = 10 ;
		field = 20.0 ;

		strcpy(linesonly, "N") ;
		strcpy(dozero, "Y") ;
		strcpy(linecolor, "white") ;
		strcpy(erase, "black") ;
		strcpy(go, "Y") ;

		here_already = 1 ;
	}

	V_clear() ;
	V_line ( 1,"  ---------------------------------------------------------------------") ;
	V_line ( 2,"    You will be viewing the area bounded by the current window:") ;
	V_line ( 5,"    SOUTH:                 NORTH:              resolution:") ;
	V_line ( 6,"    WEST:                  EAST:               lines only:") ;
	V_line ( 7,"                                               line color:") ;
	V_line ( 8,"                                               do zero:   ") ;
	V_line ( 9,"    RUN? Y/N:        ERASE? No/color:") ;
	V_line (10,"  ---------------------------------------------------------------------") ;
	V_line (11,"         Please identify your desired viewing coordinates:") ;
	V_line (13,"    Viewing from:                    Center of area to be viewed:") ;
	V_line (14,"        Northing                         Northing") ;
	V_line (15,"        Easting                          Easting") ;
	V_line (16,"        Elevation                        Elevation") ;
	V_line (18,"    Vertical Exageration") ;
	V_line (19,"    Field of View (degrees)") ;
	V_line (20,"    Line frequency") ;
	V_line (21,"  ---------------------------------------------------------------------") ;

	V_const( &(window.south),   'd',  5, 11, 10) ;
	V_const( &(window.north),   'd',  5, 34, 10) ;
	V_const( &(window.west ),   'd',  6, 11, 10) ;
	V_const( &(window.east ),   'd',  6, 34, 10) ;
	V_ques ( go             ,   's',  9, 14,  2) ;
	V_ques ( erase          ,   's',  9, 38,  8) ;
	V_ques ( &(window.ns_res ), 'd',  5, 60, 10) ;
	V_ques ( linesonly      ,   's',  6, 60,  2) ;
	V_ques ( linecolor      ,   's',  7, 60,  8) ;
	V_ques ( dozero         ,   's',  8, 60,  2) ;
	V_ques ( &from_northing ,   'd', 14, 18, 10) ;
	V_ques ( &from_easting  ,   'd', 15, 18, 10) ;
	V_ques ( &from_height   ,   'd', 16, 18, 10) ;
	V_ques ( &to_northing   ,   'd', 14, 51, 10) ;
	V_ques ( &to_easting    ,   'd', 15, 51, 10) ;
	V_ques ( &to_height     ,   'd', 16, 51, 10) ;
	V_float_accuracy(3) ;
	V_ques ( &exag          ,   'd', 18, 28,  7) ;
	V_float_accuracy(2) ;
	V_ques ( &field         ,   'd', 19, 28,  7) ;
	V_ques ( &line_freq     ,   'i', 20, 28,  5) ;

	V_call() ;
	window.ew_res = window.ns_res ;

	if (dozero[0] == 'Y' || dozero[0] == 'y')
		do_zero = 1 ;
	else
		do_zero = 0 ;

	if (linesonly[0] == 'Y' || linesonly[0] == 'y')
		lines_only = 1 ;
	else
		lines_only = 0 ;

	if (go[0] == 'Y' || go[0] == 'y')
		*do_it = 1 ;
	else
		*do_it = 0 ;

	if (erase[0] == 'N' || erase[0] == 'n')
		*erase_color = -1 ;
	else
		*erase_color = D_translate_color(erase) ;

	if (! strcmp(linecolor, "color"))
		line_color = -1 ;
	else
		line_color = D_translate_color(linecolor) ;
	if (line_color == 0)
		line_color = D_translate_color("white") ;

	return(0) ;
}
