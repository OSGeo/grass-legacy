/*  @(#)outline_map.c	1.1  5/4/87  */
#include "convert.h"
#include <stdio.h>

outline_map()
{
	int D_x_beg, D_y_beg, D_x_end, D_y_end ;
	int xbox[5] ;
	int ybox[5] ;


	D_x_beg = D_west ;
	D_x_end = D_east ;
	D_y_beg = D_north ;
	D_y_end = D_south ;

#ifdef DEBUG
	fprintf (stderr," outline map:   ")  ;
	fprintf (stderr,"D_x_beg %d  D_x_end %d\n", D_x_beg, D_x_end )  ;
	fprintf (stderr,"D_y_beg %d  D_y_end %d\n", D_y_beg, D_y_end )  ;
	getchar() ;
#endif DEBUG


	R_standard_color( D_translate_color("black") ) ;
	D_erase_window() ;

	xbox[0] = D_x_beg     ; ybox[0] = D_y_beg     ;
	xbox[1] = D_x_end     ; ybox[1] = D_y_beg     ;
	xbox[2] = D_x_end     ; ybox[2] = D_y_end     ;
	xbox[3] = D_x_beg     ; ybox[3] = D_y_end     ;
	xbox[4] = D_x_beg     ; ybox[4] = D_y_beg     ;
	R_standard_color( D_translate_color("white") ) ;
	R_polyline_abs(xbox, ybox, 5) ;
	R_flush() ;

	/*
	add_scale() ;
	*/

	R_flush() ;
}

