/*
** Written by Joshua Caplan, H. Mitasova, L. Mitas, J. Hofierka, M.Zlocha 1994
   US Army Construction Engineering Research Lab, University of Illinois 
** Copyright  Joshua Caplan, H. Mitasova, L. Mitas, J. Hofierka, M.Zlocha 1994
*/

/*
The flowtracing program, both binary and source is copyrighted, but available 
without fee for education, research and non-commercial purposes. Users may 
distribute the binary and source code to third parties provided that the
copyright notice and this statement appears on all copies and that no
charge is made for such copies.  Any entity wishing to integrate all or
part of the source code into a product for  commercial use or resale,
should contact authors of the software, U.S.Army CERL and University
of Illinois.

THE SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY. THE
U.S.Army CERL, UNIVERSITY OF ILLINOIS OR AUTHORS SHALL NOT BE LIABLE FOR 
ANY DAMAGES SUFFERED BY THE USER OF THIS SOFTWARE.

By copying this program, you, the user, agree to abide by the copyright
conditions and understandings with respect to any software which is marked
with a copyright notice.
*/

#include "r.flow.11a.h"
#include "io.11a.h"
#include "mem.11a.h"
#include "aspect.11a.h"

/**************************** PRECOMPUTATION ****************************/

void
precompute_tangents()
{
    int i;

    for (i = 0; i < 90; i++)
	tang[180 - i] = tang[360 - i] = 
	-(tang[i] = tang[i + 180] = tan((double) i * DEG2RAD));
    tang[90] = tang[270] = HUGE_VAL;
}

void
precompute_epsilons()
{
    int row;
    double x, y, t, a;

    for (row = 0; row < region.rows; row++)
    {
	x = ew_dist[row];
	y = region.ns_res;
	if (x < y)
	{
	    t = y;
	    y = x;
	    x = t;
	}
	if ((a = atan2(y, x)) <= 0.5 * DEG2RAD)
	{
	    sprintf(string, "r.flow: resolution too unbalanced (%f x %f);\
			     please resample", region.ew_res, region.ns_res);
	    G_fatal_error(string);
	}
	epsilon[HORIZ][row] = (y / tan(a - 0.5 * DEG2RAD)) - x;
	epsilon[VERT][row] = (x * tan(a + 0.5 * DEG2RAD)) - y;
#ifdef DEBUGEPS
	printf("ROW %d: HORIZ %f, VERT %f\n", row, epsilon[HORIZ][row],
	       epsilon[VERT][row]);
#endif
    }
}

void
precompute_ew_dists()
{
    int row;
    double northing;

    G_begin_distance_calculations();

    if (G_projection() == PROJECTION_LL)
	for (row = 0; row < region.rows; row++)
	{
	    northing = G_row_to_northing(row + 0.5, &region);
	    ew_dist[row] = G_distance(G_col_to_easting(0., &region), northing,
				      G_col_to_easting(1., &region), northing);
	}
    else
	for (row = 0; row < region.rows; row++)
	    ew_dist[row] = region.ew_res;
}

void
precompute_aspects()
{
    int row, col;
    double d;
    CELL *n, *c, *s;

    for (row = 0; row < region.rows; row++)
    {
	n = get_row(el, row - 1);
	c = get_row(el, row);
	s = get_row(el, row + 1);
	d = ew_dist[row];
	for (col = 0; col < region.cols; col++)
	    put(as, row, col, aspect_fly(n++, c++, s++, d));
    }
}

void
interpolate_border()
{
    int i, r = region.rows, c = region.cols;

    for (i = 0; i < c; i++)
    {
	put(el, -1, i, get(el, 0, i) * 2 - get(el, 1, i));
	put(el, r, i, get(el, r - 1, i) * 2 - get(el, r - 2, i));
    }
    for (i = 0; i < r; i++)
    {
	put(el, i, -1, get(el, i, 0) * 2 - get(el, i, 1));
	put(el, i, c, get(el, i, c - 1) * 2 - get(el, i, c - 2));
    }
    put(el, -1, -1, 3*get(el,0,0)-get(el,0,1)-get(el,1,0));
    put(el, -1, c, 3*get(el,0,c-1)-get(el,0,c-2)-get(el,1,c-1));
    put(el, r, -1, 3*get(el,r-1,0)-get(el,r-2,0)-get(el,r-1,1));
    put(el, r, c, 3*get(el,r-1,c-1)-get(el,r-2,c-1)-
	   get(el,r-1,c-2));
}

void
reflect_and_sentinel()
{
    int row, col;

    /* reflection along diagonal y = x, sentineling of 0 cells */
    for (row = 0; row < region.rows; row++)
	for (col = 0; col < region.cols; col++)
	    if (aspect(row, col) == 0)
		put(as, row, col, (int) UNDEF);
	    else if (aspect(row, col) < 90)
		put(as, row, col, 90 - aspect(row, col));
	    else
		put(as, row, col, 450 - aspect(row, col));
}

void
upslope_correction()
{
    int row, col;

    for (row = 0; row < region.rows; row++)
	for (col = 0; col < region.cols; col++)
	    put(el, row, col, -get(el, row, col));

    /* rotation of 180 degrees */
    if (parm.aspin)
	for (row = 0; row < region.rows; row++)
	    for (col = 0; col < region.cols; col++)
		if (aspect(row, col) <= 180)
		    put(as, row, col, aspect(row, col) + 180);
		else if (aspect(row, col) <= 360)
		    put(as, row, col, aspect(row, col) - 180);
}

void
precompute()
{
    diag("Precomputing: tangents");
    precompute_tangents();
    diag(", e/w distances");
    precompute_ew_dists();
    diag(", quantization tolerances");
    precompute_epsilons();
    if (parm.up)
    {
	diag(", inverted elevations");
	upslope_correction();
    }
    if (!parm.aspin)
    {
	diag(", interpolated border elevations");
	interpolate_border();
    }
    if (!parm.mem)
	if (parm.aspin)
	{
	    diag(", re-oriented aspects");
	    reflect_and_sentinel();
	}
	else
	{
	    diag(", aspects");
	    precompute_aspects();
	}

    diag(".\n");
}
