#include <stdio.h>
#include <string.h>
#include "options.h"
#include "display.h"
#include "raster.h"
#include "colors.h"
#include "local_proto.h"

#define CHUNK	128

static int coors_allocated = 0 ;
static int *xarray ;
static int *yarray ;

static float xincr ;
static float yincr ;

int set_graph_stuff (void)
{
	xincr = (float)(r - l)/100. ;
	if (xincr < 0.0) xincr = -xincr; /* mod: shapiro 13 jun 1991 */
	yincr = (float)(b - t)/100. ;
	if (yincr < 0.0) yincr = -yincr; /* mod: shapiro 13 jun 1991 */

	return 0;
}

int set_text_size (void)
{
	if (  hsize>=0.
	   && vsize>=0.
	   && hsize<=100.
	   && vsize<=100.)
	R_text_size((int)(hsize * xincr), (int)(vsize * yincr)) ;
	return(0) ;
}

int do_draw (char *buff)
{
	float xper, yper ;

	if ( 2 != sscanf(buff, "%*s %f %f", &xper, &yper) )
		return(-1) ;
	if (  xper<0.
	   || yper<0.
	   || xper>100.
	   || yper>100.)
		return(-1) ;

	R_cont_abs(l + (int)(xper * xincr), b - (int)(yper * yincr)) ;
	return(0) ;
}

int do_move (char *buff)
{
	float xper, yper ;

	if ( 2 != sscanf(buff, "%*s %f %f", &xper, &yper) )
		return(-1) ;
	if (  xper<0.
	   || yper<0.
	   || xper>100.
	   || yper>100.)
		return(-1) ;

	R_move_abs(l + (int)(xper * xincr), b - (int)(yper * yincr)) ;
	return(0) ;
}

int do_color (char *buff)
{
	char in_color[64] ;
	int R, G, B, color = 0;
	const int customcolor = MAX_COLOR_NUM + 1;

	sscanf(buff, "%*s %s", in_color) ;

	/* Parse and select color */
	color = G_str_to_color(in_color, &R, &G, &B);
	if(color == 0) {
	    G_warning("[%s]: No such color", in_color);
	    return(-1);
	}
	if(color == 1) {
	    R_reset_color(R, G, B, customcolor);
	    R_color(customcolor);
	}
	if(color == 2) {  /* color == 'none' */
	    R = D_translate_color(DEFAULT_BG_COLOR);
	    R_standard_color(R);
	}

	return(0);
}

char *do_poly (char *buff, FILE *infile)
{
	int num ;
	char origcmd[64] ;
	float xper, yper ;
	char *fgets() ;
	char *to_return ;

	sscanf(buff, "%s", origcmd) ;

	num = 0 ;

	for(;;)
	{
		if ( (to_return = fgets(buff, 128, infile)) == NULL)
			break ;

		if (! sscanf(buff, "%f %f", &xper, &yper) )
			break ;

		if (  xper<0.
		   || yper<0.
		   || xper>100.
		   || yper>100.)
			break ;

		check_alloc(num+1) ;
		xarray[num] = l + (int)(xper * xincr) ;
		yarray[num] = b - (int)(yper * yincr) ;
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

int do_size (char *buff)
{
	float xper, yper ;

	if ( 2 != sscanf(buff, "%*s %f %f", &xper, &yper) )
		return(-1) ;
	if (  xper<0.
	   || yper<0.
	   || xper>100.
	   || yper>100.)
		return(-1) ;

	R_text_size((int)(xper * xincr), (int)(yper * yincr)) ;
	return(0) ;
}

int do_text (char *buff)
{
	char *ptr ;

/* remove new line */
	for( ptr=buff; *ptr != 012; ptr++) ;
	*ptr = '\0' ;

	ptr = buff ;
	for(; *ptr != ' '; ptr++) ;
	for(; *ptr == ' '; ptr++) ;
	R_text(ptr) ;

	return 0;
}

int check_alloc (int num)
{
	int to_alloc ;

	if (num < coors_allocated)
		return 0;
	
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

	return 0;
}

int do_icon (char *buff)
{
	double xper, yper ;
	char type ;
	int size ;
	int ix, iy ;

	if ( 4 != sscanf(buff, "%*s %c %d %lf %lf", &type, &size, &xper, &yper) )
		return(-1) ;

	if (  xper<0.
	   || yper<0.
	   || xper>100.
	   || yper>100.)
		return(-1) ;

	ix = l + (int)(xper * xincr) ;
	iy = b - (int)(yper * yincr) ;

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
