/*  @(#)redraw.c	2.1  6/26/87  */
#include <stdio.h>

redraw_window(f_digit)
	FILE *f_digit ;
{
	double thresh ;

	R_standard_color( D_translate_color("black")) ;
	D_erase_window() ;
	outline_window() ;
	read_digit_head(f_digit, &thresh) ;
	replot(f_digit) ;
}
