#include <stdio.h>
#include <string.h>
#include <grass/gis.h>
#include "options.h"
#include <grass/display.h>
#include <grass/raster.h>
#include <grass/glocale.h>

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
	{
	    R_text_size((int)(hsize * xincr), (int)(vsize * yincr)) ;
	    G_debug(3,"text size initialized to [%d,%d] pixels",
		(int)(hsize * xincr), (int)(vsize * yincr));
	}
	return(0) ;
}

int do_draw (char *buff)
{
	float xper, yper ;

	if ( 2 != sscanf(buff, "%*s %f %f", &xper, &yper) ) {
	    G_warning(_("Problem parsing coordinates [%s]"), buff);
	    return(-1);
	}

	if(mapunits) {
/* skip check: clips segments if map coordinate is out of region.
	    if( xper < D_get_u_west() ||
		yper < D_get_u_south() ||
		xper > D_get_u_east() ||
		yper > D_get_u_north() )
		  return(-1);
*/
	    R_cont_abs((int)(D_u_to_d_col(xper)+0.5), (int)(D_u_to_d_row(yper)+0.5));
	}
	else {
	    if( xper<0. || yper<0. || xper>100. || yper>100. )
		return(-1);
	    R_cont_abs(l + (int)(xper * xincr), b - (int)(yper * yincr)) ;
	}

	return(0) ;
}

int do_move (char *buff)
{
	float xper, yper ;

	if ( 2 != sscanf(buff, "%*s %f %f", &xper, &yper) ) {
	    G_warning(_("Problem parsing coordinates [%s]"), buff);
	    return(-1);
	}

	if(mapunits)
	    R_move_abs((int)(D_u_to_d_col(xper)+0.5), (int)(D_u_to_d_row(yper)+0.5));
	else {
	    if( xper<0. || yper<0. || xper>100. || yper>100. )
		return(-1);
	    R_move_abs(l + (int)(xper * xincr), b - (int)(yper * yincr));
	}

	return(0) ;
}

int do_color (char *buff)
{
	char in_color[64] ;
	int R, G, B, color = 0;

	if ( 1 != sscanf(buff, "%*s %s", in_color) ) {
	    G_warning(_("Unable to read color"));
	    return(-1);
	}

	/* Parse and select color */
	color = G_str_to_color(in_color, &R, &G, &B);
	if(color == 0) {
	    G_warning(_("[%s]: No such color"), in_color);
	    return(-1);
	}
	if(color == 1) {
	    R_RGB_color(R, G, B);
	}
	if(color == 2) {  /* color == 'none' */
	    R = D_translate_color(DEFAULT_BG_COLOR);
	    R_standard_color(R);
	}

	return(0);
}

int do_linewidth(char *buff)
{
	int width; /* in pixels */

	if ( 1 != sscanf(buff, "%*s %d", &width) ) {
	    G_warning(_("Problem parsing command [%s]"), buff);
	    return(-1);
	}

	R_line_width(width);
	G_debug(3,"line width set to %d pixels", width);	

	return(0);
}


int do_poly (char *buff, FILE *infile)
{
	int num ;
	char origcmd[64] ;
	float xper, yper ;
	char *fgets() ;
	int to_return ;

	sscanf(buff, "%s", origcmd) ;

	num = 0 ;

	for(;;)
	{
		if ( (to_return = G_getl2(buff, 128, infile)) != 1)
			break ;

		if (2 != sscanf(buff, "%f %f", &xper, &yper) ) {

		    if( '#' == buff[0] ) {
			G_debug(3," skipping comment line [%s]", buff);
			continue;
		    }

		    G_debug(3,"coordinate pair not found. ending polygon. [%s]", buff);
		    break;
		}

		if(!mapunits) {
		    if( xper<0. || yper<0. || xper>100. || yper>100.)
			break;
		}
		check_alloc(num+1) ;

		if(mapunits) {
		    xarray[num] = (int)(D_u_to_d_col(xper)+0.5);
		    yarray[num] = (int)(D_u_to_d_row(yper)+0.5);
		}
		else {
		    xarray[num] = l + (int)(xper * xincr);
		    yarray[num] = b - (int)(yper * yincr);
		}

		num++ ;
	}

	if (num)
	{
	    /* this check is here so you can use the "polyline" command 
		 to make an unfilled polygon */
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
	int ret;

	ret = sscanf(buff, "%*s %f %f", &xper, &yper);

	if( ret != 2 && ret != 1 ) {
	    G_warning(_("Problem parsing command [%s]"), buff);
	    return(-1);
	}

	/* if only one size is given assume same value in both axes */
	if( ret == 1 ) yper = xper;

	if (  xper<0.
	   || yper<0.
	   || xper>100.
	   || yper>100.)
		return(-1) ;

	R_text_size((int)(xper * xincr), (int)(yper * yincr)) ;
	G_debug(3,"text size set to [%d,%d] pixels",
	    (int)(xper * xincr), (int)(yper * yincr));

	return(0) ;
}

int do_text_rotate (char *buff)
{
	float rotation; /* degrees counter-clockwise from east */

	if ( 1 != sscanf(buff, "%*s %f", &rotation) ) {
	    G_warning(_("Problem parsing command [%s]"), buff);
	    return(-1);
	}

	R_text_rotation(rotation);
	G_debug(3,"text rotation set to %.1f degrees", rotation);	

	return(0);
}

int do_text (char *buff)
{
	char *ptr ;

	ptr = buff ;
	/* skip to beginning of actual text */
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

	if ( 4 != sscanf(buff, "%*s %c %d %lf %lf", &type, &size, &xper, &yper) ) {
	    G_warning(_("Problem parsing command [%s]"), buff);
	    return(-1);
	}

	if(mapunits) {
	    ix = (int)(D_u_to_d_col(xper)+0.5);
	    iy = (int)(D_u_to_d_row(yper)+0.5);
	    /* size in map units too? currently in percentage.
		use "size * D_get_u_to_d_yconv()" to convert? */
	}
	else {
	    if( xper<0. || yper<0. || xper>100. || yper>100.)
		return(-1) ;

	    ix = l + (int)(xper * xincr) ;
	    iy = b - (int)(yper * yincr) ;
	}

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
