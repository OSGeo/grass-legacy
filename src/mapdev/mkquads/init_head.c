
#include <stdio.h>
#include "gis.h"
#include "dig_head.h"

init_header (fp_digit, window)
	FILE  *fp_digit ;
	struct Cell_head *window ;
{

	strcpy(head.organization, "US Army Const. Eng. Rsch. Lab") ;
	head.orig_scale = 0 ;
	head.digit_thresh = 0.0 ;
	head.map_thresh = 0.0 ;
	head.plani_zone = window->zone ;

/*  load default window settings into digit header  */
	head.W = window->west ;
	head.E = window->east ;
	head.S = window->south ;
	head.N = window->north ;

	dig_write_head_binary( fp_digit, &head) ;

}

