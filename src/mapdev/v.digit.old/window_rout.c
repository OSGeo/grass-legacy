#include "digit.h"
#include "gis.h"
#include "graphics.h"

double floor(), ceil();


window_rout (N, S, E, W)
    double N, S, E, W;
{
    if (strcmp (N_backdrop, "None"))
	fit_window (&N, &S, &E, &W);
    window_conversions (N, S, E, W);
}

/* 
**
**  this routine only gets called when re-windowing AND we have
**  a backdrop cell map.  This routine adjusts the window
**  to match up evenly on cell boundaries so the vector and
**  cell data will agree.
*/
fit_window (N, S, E, W)
    double *N, *S, *E, *W;
{
    struct Cell_head src, dst;
    double col, row;

/*DEBUG*/ debugf ("FITWIN Entry: (%lf, %lf, %lf, %lf)\n", *N, *S, *E, *W);
    if (0 > G_get_cellhd (N_backdrop, N_backdrop_mapset, &src))
	return (-1);

    /* adjust to edges of cells */
    /* note this will not allow one to zoom in farther than 
    **  one cell, and at worst 4, if crossing cell borders
    */
    {
	col = (*W - src.west) / src.ew_res;
	*W = floor (col) * src.ew_res + src.west;

	col = (*E - src.west) / src.ew_res;
	*E = ceil (col) * src.ew_res + src.west;

	row = (src.north - *S) / src.ns_res;
	*S = src.north - ceil (row) * src.ns_res;

	row = (src.north - *N) / src.ns_res;
	*N = src.north - floor (row) * src.ns_res;
    }

    src.north = *N;
    src.south = *S;
    src.east  = *E;
    src.west  = *W;
/*DEBUG*/ debugf ("FITWIN PRE-adj: (%lf, %lf, %lf, %lf)\n", src.north, src.south, src.east, src.west);
    src.rows  = (src.north - src.south) / src.ns_res;
    src.cols  = (src.east  - src.west ) / src.ew_res;
/*DEBUG*/ debugf ("FITWIN PRE-adj screen: row %d col %d (%d, %d, %d, %d)\n", src.rows, src.cols, screen_top, screen_bot, screen_left, screen_right);
/*DEBUG*/ debugf ("PRE-adj  NSres = %lf   WEres = %lf\n", src.ns_res, src.ew_res);
    G_adjust_window_to_box (&src, &dst, screen_bot - screen_top, 
	screen_right - screen_left);
    
    /* center the information */



    G_set_window (&dst);

/*DEBUG*/ debugf ("FITWIN EXIT1: (%lf, %lf, %lf, %lf)\n", src.north, src.south, src.east, src.west);
/*DEBUG*/ debugf ("FITWIN EXIT2: (%lf, %lf, %lf, %lf)\n", dst.north, dst.south, dst.east, dst.west);
    return (0);
}
