/*  @(#)show_line.c	2.1  6/26/87  */
#include <stdio.h>
#include "structures.h"
#include "arrays.h"
#include "mode.h"

show_line(f_digit, line, color)
	FILE *f_digit ;
	char *line;
	char *color;
{
	int at_line ;
	int n_points ;

	at_line = abs(line) ;
	fseek(f_digit, lines[at_line].offset, 0) ;
	n_points = lines[at_line].n_points ;
	if (0 >= fread(xarray, sizeof(double), n_points, f_digit) )
		return(-1) ;
	if (0 >= fread(yarray, sizeof(double), n_points, f_digit) )
		return(-1) ;
	
	xarray[0] = endpoints[lines[at_line].endpoint_beg].x ;
	yarray[0] = endpoints[lines[at_line].endpoint_beg].y ;
	xarray[n_points-1] = endpoints[lines[at_line].endpoint_end].x ;
	yarray[n_points-1] = endpoints[lines[at_line].endpoint_end].y ;
	
	plot_points(LINE, n_points, xarray, yarray, color, color) ;
	return(0) ;
}
