/*  %W%  %G%  */

#include "options.h"
#include <stdio.h>

#define CHUNK	128

static int coors_allocated = 0 ;
static int *xarray ;
static int *yarray ;

static double xincr ;
static double yincr ;

static double cur_x = 0. ;
static double cur_y = 0. ;

char *falloc() ;
char *frealloc() ;

extern double D_u_to_d_col() ;
extern double D_u_to_d_row() ;

extern struct Cell_head window ;

set_text_size()
{
	double x, y ;

	x = D_u_to_d_col((double)0) - D_u_to_d_col(hsize) ;
	y = D_u_to_d_row((double)0) - D_u_to_d_row(vsize) ;

	R_text_size(abs((int)x), abs((int)y)) ;
	return(0) ;
}

do_draw(buff)
	char *buff ;
{
	double x, y ;

	if ( 2 != sscanf(buff, "%*s %lf %lf", &x, &y) )
		return(-1) ;
	if (bad_coor(x, y))
		return(-1) ;

	R_cont_abs( (int)D_u_to_d_col(x), (int)D_u_to_d_row(y) ) ;
	return(0) ;
}

do_move(buff)
	char *buff ;
{
	double x, y ;

	if ( 2 != sscanf(buff, "%*s %lf %lf", &x, &y) )
		return(-1) ;
	if (bad_coor(x, y))
		return(-1) ;

	R_move_abs( (int)D_u_to_d_col(x), (int)D_u_to_d_row(y) ) ;
	return(0) ;
}

do_icon(buff)
	char *buff ;
{
	double x, y ;
	int ix, iy ;
	char type ;
	int size ;

	if ( 4 != sscanf(buff, "%*s %c %d %lf %lf", &type, &size, &x, &y) )
		return(-1) ;
	if (bad_coor(x, y))
		return(-1) ;

	ix = (int)D_u_to_d_col(x) ;
	iy = (int)D_u_to_d_row(y) ;
	switch (type & 0177)
	{
	case 'o':
		R_move_abs( ix-size, iy-size) ;
		R_cont_abs( ix-size, iy+size) ;
		R_cont_abs( ix+size, iy+size) ;
		R_cont_abs( ix+size, iy-size) ;
		R_cont_abs( ix-size, iy-size) ;
		break ;
	case 'x':
		R_move_abs( ix-size, iy-size) ;
		R_cont_abs( ix+size, iy+size) ;
		R_move_abs( ix-size, iy+size) ;
		R_cont_abs( ix+size, iy-size) ;
		break ;
	case '+':
	default:
		R_move_abs( ix     , iy-size) ;
		R_cont_abs( ix     , iy+size) ;
		R_move_abs( ix-size, iy     ) ;
		R_cont_abs( ix+size, iy     ) ;
		break ;
	}
	return(0) ;
}

do_color(buff)
	char *buff ;
{
	char color[64] ;
	int colr ;

	sscanf(buff, "%*s %s", color) ;
	colr = D_translate_color(color) ;
	if (colr == 0)
		return(-1) ;
	R_standard_color(colr) ;
	return(0) ;
}

char *
do_poly(buff, infile)
	char *buff ;
	FILE *infile ;
{
	int num ;
	char origcmd[64] ;
	double x, y ;
	char *fgets() ;
	char *to_return ;

	sscanf(buff, "%s", origcmd) ;

	num = 0 ;

	for(;;)
	{
		if ( (to_return = fgets(buff, 128, infile)) == NULL)
			break ;

		if (! sscanf(buff, "%lf %lf", &x, &y) )
			break ;

		check_alloc(num+1) ;
		xarray[num] = (int)D_u_to_d_col(x) ;
		yarray[num] = (int)D_u_to_d_row(y) ;
		num++ ;
	}

	if (num)
	{
		if(! strcmp(origcmd, "polygon"))
			R_polygon_abs(xarray, yarray, num) ;
		else
			R_polyline_abs(xarray, yarray, num) ;
	}

	return(to_return) ;
}

do_size(buff)
	char *buff ;
{
	double x, y ;

	if ( 2 != sscanf(buff, "%*s %lf %lf", &hsize, &vsize) )
		return(-1) ;
	
	set_text_size() ;
}

do_text(buff)
	char *buff ;
{
	char *ptr ;

/* remove new line */
	for( ptr=buff; *ptr != 012; ptr++) ;
	*ptr = 0 ;

	ptr = buff ;
	for(; *ptr != ' '; ptr++) ;
	for(; *ptr == ' '; ptr++) ;
	R_text(ptr) ;
}

check_alloc(num)
	int num ;
{
	int to_alloc ;

	if (num < coors_allocated)
		return ;
	
	to_alloc = coors_allocated ;
	while (num >= to_alloc)
		to_alloc += CHUNK ;

	if (coors_allocated == 0)
	{
		xarray = (int *) falloc(to_alloc, sizeof(int)) ;
		yarray = (int *) falloc(to_alloc, sizeof(int)) ;
	}
	else
	{
		xarray = (int *)frealloc(
			(char *)xarray,
			to_alloc,
			sizeof(int),
			coors_allocated) ;
		yarray = (int *)frealloc(
			(char *)yarray,
			to_alloc,
			sizeof(int),
			coors_allocated) ;
	}
	
	coors_allocated = to_alloc ;
}

static
bad_coor(x, y)
	double x, y ;
{
	if (x < window.west)
		return(-1) ;
	if (x > window.east)
		return(-1) ;
	if (y < window.south)
		return (-1) ;
	if (y > window.north)
		return (-1) ;
	return(0) ;
}
