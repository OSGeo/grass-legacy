/*
**  Original Algorithm:    H. Mitasova, L. Mitas, J. Hofierka, M. Zlocha 
**  GRASS Implementation:  J. Caplan, M. Ruesink  1995
**
**  US Army Construction Engineering Research Lab, University of Illinois 
**
**  Copyright  M. Ruesink, J. Caplan, H. Mitasova, L. Mitas, J. Hofierka, 
**	M. Zlocha  1995
**
**This program is free software; you can redistribute it and/or
**modify it under the terms of the GNU General Public License
**as published by the Free Software Foundation; either version 2
**of the License, or (at your option) any later version.
**
**This program is distributed in the hope that it will be useful,
**but WITHOUT ANY WARRANTY; without even the implied warranty of
**MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**GNU General Public License for more details.
**
**You should have received a copy of the GNU General Public License
**along with this program; if not, write to the Free Software
**Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
**
*/


#include "r.flow.13.h"
#include "io.13.h"
#include "mem.13.h"
#include "aspect.13.h"

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
		/* put(as, row, col, (int) UNDEF); */
		G_set_d_null_value(&(as.buf[row][col]),1);
	    else if (aspect(row, col) < 90)
		put(as, row, col, 90 - aspect(row, col));
	    else
		put(as, row, col, 450 - aspect(row, col));
}

void
precompute_aspects()
{
    int row, col;
    double d;
    DCELL *n, *c, *s, temp;

    for (row = 0; row < region.rows; row++)
    {
	n = get_row(el, row - 1);
	c = get_row(el, row);
	s = get_row(el, row + 1);
	d = ew_dist[row];
	/* for (col = 0; col < region.cols; col++)
	    put(as, row, col, aspect_fly(n++, c++, s++, d)); */
	for (col = 0; col < region.cols; col++) {
	    temp = aspect_fly(n++, c++, s++, d);
	    if (temp == UNDEF)
		G_set_d_null_value(&(as.buf[row][col]), 1);
	    else
		put(as, row, col, temp);
	}
    }
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




