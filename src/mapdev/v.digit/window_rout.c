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
    struct Cell_head src, dst, tmp_wind;
    double col, row;

/*DEBUG*/ debugf ("FITWIN Entry: (%lf, %lf, %lf, %lf)\n", *N, *S, *E, *W);
    if (0 > G_get_cellhd (N_backdrop, N_backdrop_mapset, &src))
	return (-1);
    G_copy (&tmp_wind, &src, sizeof(src));
    tmp_wind.north = *N;
    tmp_wind.south = *S;
    tmp_wind.east = *E;
    tmp_wind.west = *W;

    /* adjust to edges of cells */
    /* note this will not allow one to zoom in farther than 
    **  one cell, and at worst 4, if crossing cell borders
    */

    G_align_window(&tmp_wind, &src);
    G_copy (&src, &tmp_wind, sizeof(src));

    *N = tmp_wind.north;
    *S = tmp_wind.south;
    *E = tmp_wind.east;
    *W = tmp_wind.west;

/*DEBUG*/ debugf ("FITWIN PRE-adj: (%lf, %lf, %lf, %lf)\n", src.north, src.south, src.east, src.west);
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
