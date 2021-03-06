/*!
   \file buffer.c

   \brief Vector library - nearest, adjust, parallel lines

   Higher level functions for reading/writing/manipulating vectors.

   (C) 2001-2011 by the GRASS Development Team

   This program is free software under the 
   GNU General Public License (>=v2). 
   Read the file COPYING that comes with GRASS
   for details.

   \author Radim Blazek
   \author Markus Metz

   \date 2001-2011
 */

/* BUGS
 * 
 * buffering these shapes is unresolved for buffer distances which 
 * should create a closed loop in the inside
 * 
 *  -----------   ------------
 *  |          \ /              
 *  |
 *  |
 *  |          / \
 *  -----------   ------------
 * 
 * 
 *  -----------
 *  |         |
 *  |          
 *  |         | 
 *  -----------
 * 
 * 
 * for certain buffer distances, undefined behaviour for
 * 
 *  ---------
 *  |       |
 *  |
 *  --------------
 *               |
 *  --------------
 *  |
 *  |       |
 *  ---------
 * 
 * MM April 2011
 * */

#include <stdlib.h>
#include <math.h>
#include <grass/Vect.h>
#include <grass/gis.h>

#define LENGTH(DX, DY)  (  sqrt( (DX*DX)+(DY*DY) )  )
#define PI M_PI
#define D_MULT 0.99999999  /* distance multiplier for point_in_buf() */

/* vector() calculates normalized vector from two points */
static void vect(double x1, double y1, double x2, double y2, double *x,
		 double *y)
{
    double dx, dy, l;

    dx = x2 - x1;
    dy = y2 - y1;
    l = LENGTH(dx, dy);
    if (l == 0) {
	/* assume that dx == dy == 0, which should give (NaN,NaN) */
	/* without this, very small dx or dy could result in Infinity */
	dx = dy = 0;
    }
    *x = dx / l;
    *y = dy / l;
}

/* find_cross find first crossing between segments from s1 to s2 and from s3 to s4
 ** s5 is set to first segment and s6 to second
 ** neighbours are taken as crossing each other only if overlap
 ** returns: 1 found
 **         -1 found overlap
 **          0 not found
 */
static int find_cross(struct line_pnts *Points, int s1, int s2, int s3,
		      int s4, int *s5, int *s6)
{
    int i, j, jstart, np, ret;
    double *x, *y;

    G_debug(5,
	    "find_cross(): npoints = %d, s1 = %d, s2 = %d, s3 = %d, s4 = %d",
	    Points->n_points, s1, s2, s3, s4);

    x = Points->x;
    y = Points->y;
    np = Points->n_points;

    for (i = s1; i <= s2; i++) {
	jstart = i + 1 < s3 ? s3 : i + 1;
	for (j = jstart; j <= s4; j++) {
	    if (j == i) {
		continue;
	    }
	    ret =
		dig_test_for_intersection(x[i], y[i], x[i + 1], y[i + 1],
					  x[j], y[j], x[j + 1], y[j + 1]);
	    /* j should always be > i */
	    if (ret == 1 && ((i - j) > 1 || (i - j) < -1)) {
		*s5 = i;
		*s6 = j;
		G_debug(5, "  intersection: s5 = %d, s6 = %d", *s5, *s6);
		return 1;
	    }
	    if (ret == -1) {
		*s5 = i;
		*s6 = j;
		G_debug(5, "  overlap: s5 = %d, s6 = %d", *s5, *s6);
		return -1;
	    }
	}
    }
    G_debug(5, "find_cross() ->  no intersection");
    return 0;
}

/* find_cross2 find first crossing between segments from s1 to s2 and from s3 to s4
 ** proceed from s1 to s2 and from s4 to s3, i.e. forward/backward
 ** s5 is set to first segment and s6 to second
 ** neighbours are taken as crossing each other only if overlap
 ** returns: 1 found
 **         -1 found overlap
 **          0 not found
 */
static int find_cross_reverse(struct line_pnts *Points, int s1, int s2, int s3,
		      int s4, int *s5, int *s6)
{
    int i, j, jend, np, ret;
    double *x, *y;

    G_debug(5, 
	    "find_cross_reverse(): npoints = %d, s1 = %d, s2 = %d, s3 = %d, s4 = %d",
	    Points->n_points, s1, s2, s3, s4);

    x = Points->x;
    y = Points->y;
    np = Points->n_points;

    for (i = s1; i <= s2; i++) {
	jend = i + 1 < s3 ? s3 : i + 1;
	for (j = s4 - 1; j >= jend; j--) {
	    if (j == i) {
		continue;
	    }
	    ret =
		dig_test_for_intersection(x[i], y[i], x[i + 1], y[i + 1],
					  x[j], y[j], x[j + 1], y[j + 1]);
	    /* j should always be >= i */
	    if (ret == 1 && ((i - j) > 1 || (i - j) < -1)) {
		*s5 = i;
		*s6 = j;
		G_debug(5, "  intersection: s5 = %d, s6 = %d", *s5, *s6);
		return 1;
	    }
	    if (ret == -1) {
		*s5 = i;
		*s6 = j;
		G_debug(5, "  overlap: s5 = %d, s6 = %d", *s5, *s6);
		return -1;
	    }
	}
    }
    G_debug(5, "find_cross_reverse() ->  no intersection");
    return 0;
}

/* find_cross_from_start find first crossing of segment s1 to s1 + 1 with all
 ** segments from s1 + 1 to Points->n_points - 2
 ** ix is set to East crossing and iy to North crossing
 ** s2 is set to the intersecting segment
 ** neighbours are taken as crossing each other only if overlap
 ** returns: 1 found
 **          0 not found
 */
static int find_cross_from_start(struct line_pnts *Points, int s1, int *s2,
                                 double *ix, double *iy)
{
    int i, np, ret, found = 0;
    double *x, *y, min_dist, new_dist, new_x, new_y, dx, dy;

    G_debug(5, "find_cross_from_start(): npoints = %d, s1 = %d",
                                                  Points->n_points, s1);

    x = Points->x;
    y = Points->y;
    np = Points->n_points;
    
    min_dist = PORT_DOUBLE_MAX;

    for (i = np - 2; i > s1; i--) {
	ret =
	    dig_test_for_intersection(x[s1], y[s1], x[s1 + 1], y[s1 + 1],
				      x[i], y[i], x[i + 1], y[i + 1]);
				      
	if (ret) {
	    dig_find_intersection(x[s1], y[s1], x[s1 + 1], y[s1 + 1], x[i],
				  y[i], x[i + 1], y[i + 1], &new_x, &new_y);
	    if ((new_x != x[s1] || new_y != y[s1]) && 
	        (new_x != x[s1 + 1] || new_y != y[s1 + 1])) {
		found = 1;
		dx = x[s1] - new_x;
		dy = y[s1] - new_y;
		new_dist = LENGTH(dx, dy);
		if (min_dist > new_dist) {
		    min_dist = new_dist;
		    *ix = new_x;
		    *iy = new_y;
		    *s2 = i;
		}
	    }
	}
    }

    G_debug(5, "find_cross_from_start(): intersection %s",
                                         found ? "found" : "not found");
    return found;
}

/* find_cross_from_end find first crossing of segment s1 to s1 + 1 with all
 ** segments from 0 to s1 - 1
 ** ix is set to East crossing and iy to North crossing
 ** s2 is set to the intersecting segment
 ** neighbours are taken as crossing each other only if overlap
 ** returns: 1 found
 **          0 not found
 */
static int find_cross_from_end(struct line_pnts *Points, int s1, int *s2,
                               double *ix, double *iy)
{
    int i, np, ret, found = 0;
    double *x, *y, min_dist, new_dist, new_x, new_y, dx, dy;

    G_debug(5, "find_cross_from_end(): npoints = %d, s1 = %d",
                                                  Points->n_points, s1);

    x = Points->x;
    y = Points->y;
    np = Points->n_points;
    
    min_dist = PORT_DOUBLE_MAX;

    for (i = 0; i < s1; i++) {
	ret =
	    dig_test_for_intersection(x[s1], y[s1], x[s1 + 1], y[s1 + 1],
				      x[i], y[i], x[i + 1], y[i + 1]);
				      
	if (ret) {
	    dig_find_intersection(x[s1], y[s1], x[s1 + 1], y[s1 + 1], x[i],
				  y[i], x[i + 1], y[i + 1], &new_x, &new_y);
	    if ((new_x != x[s1] || new_y != y[s1]) && 
	        (new_x != x[s1 + 1] || new_y != y[s1 + 1])) {
		found = 1;
		dx = x[s1 + 1] - new_x;
		dy = y[s1 + 1] - new_y;
		new_dist = LENGTH(dx, dy);
		if (min_dist > new_dist) {
		    min_dist = new_dist;
		    *ix = new_x;
		    *iy = new_y;
		    *s2 = i;
		}
	    }
	}
    }

    G_debug(5, "find_cross_from_end(): intersection %s",
                                         found ? "found" : "not found");
    return found;
}

/* point_in_buf - test if point px,py is in d buffer of Points
 ** returns:  1 in buffer
 **           0 not  in buffer
 */
static int point_in_buf(struct line_pnts *Points, double px, double py,
			double d)
{
    int i, np;
    double sd;

    np = Points->n_points;
    /* d must be the squared distance */
    for (i = 0; i < np - 1; i++) {
	sd = dig_distance2_point_to_line(px, py, 0,
					 Points->x[i], Points->y[i], 0,
					 Points->x[i + 1], Points->y[i + 1],
					 0, 0, NULL, NULL, NULL, NULL, NULL);
	if (sd <= d) {
	    return 1;
	}
    }
    return 0;
}

/* clean_parallel - clean parallel line created by parallel_line:
 ** - looking for loops and if loop doesn't contain any other loop
 **   and centroid of loop is in buffer removes this loop (repeated)
 ** - optionally removes all end points in buffer
 ** - optionally closes output line if input line is looped
 *    parameters:
 *      Points - parallel line
 *      origPoints - original line
 *      d - offset
 *      rm_end - remove end points in buffer
 ** note1: on some lines (multiple selfcrossing; lines with end points
 **        in buffer of other line; some shapes of ends ) may create nosense
 ** note2: this function is stupid and slow, somebody more clever
 **        than I am should write paralle_line + clean_parallel
 **        better;    RB March 2000
 **        speed a bit improved, more thorough cleaning    MM Feb 2011
 */
static void clean_parallel(struct line_pnts *Points,
			   struct line_pnts *origPoints,
			   double d, double tol, int rm_end, int close_loop)
{
    int i, j, np, npn, sa, sb, ret, side;
    int sa_max = 0;
    int first = 0, current, last, lcount;
    double *x, *y, px, py, ix, iy;
    static struct line_pnts *sPoints = NULL;
    double d2 = d * d * D_MULT;

    G_debug(4, "clean_parallel(): npoints = %d, d = %f, rm_end = %d",
	    Points->n_points, d, rm_end);

    Vect_line_prune(Points);
    x = Points->x;
    y = Points->y;
    np = Points->n_points;
    
    if (np < 3)
	return;
	
    /* duplicate consecutive points cause problems for
     * dig_test_for_intersection() and dig_find_intersection()
     * -> output line must always be pruned */

    if (rm_end) {
	int inserted;

	/* remove points from end in buffer */
	j = inserted = 0;
	for (i = Points->n_points - 1; i >= 1; i--) {
	    x = Points->x;
	    y = Points->y;
	    if (rm_end < 2) {
		px = (x[i] + x[i - 1]) / 2;
		py = (y[i] + y[i - 1]) / 2;
	    }
	    else {
		/* this is for the last arc created by Vect_line_buffer() */
		px = x[i - 1];
		py = y[i - 1];
	    }
	    if (point_in_buf(origPoints, x[i], y[i], d2)) {
		
		/* check for intersection of this segment */
		/* must not be the first or last point of this segment */
		current = i - 1;
		if (find_cross_from_end(Points, current, &sa, &ix, &iy) != 0) {
		    /* insert intersection point into this segment */
		    /* and adjust i and Points->n_points */
		    int k = 0;

		    if ((ix == x[sa] && iy == y[sa]) ||
		        (ix == x[sa + 1] && iy == y[sa + 1])) {
			    
			/* do not insert ix,iy into sa segment */
			Vect_append_point(Points, Points->x[Points->n_points - 1],
					  Points->y[Points->n_points - 1], 0);
			
			for (k = Points->n_points - 2; k > current + 1; k--) {
			    Points->x[k] = Points->x[k - 1];
			    Points->y[k] = Points->y[k - 1];
			}
			Points->x[current + 1] = ix;
			Points->y[current + 1] = iy;
			i += 2;
		    }
		    else {
			Vect_append_point(Points, Points->x[Points->n_points - 1],
					  Points->y[Points->n_points - 1], 0);
			Vect_append_point(Points, Points->x[Points->n_points - 1],
					  Points->y[Points->n_points - 1], 0);
			
			for (k = Points->n_points - 2; k > current + 2; k--) {
			    Points->x[k] = Points->x[k - 2];
			    Points->y[k] = Points->y[k - 2];
			}
			Points->x[current + 2] = ix;
			Points->y[current + 2] = iy;
			for (k = current + 1; k > sa + 1; k--) {
			    Points->x[k] = Points->x[k - 1];
			    Points->y[k] = Points->y[k - 1];
			}
			Points->x[sa + 1] = ix;
			Points->y[sa + 1] = iy;
			i += 3;
		    }

		    inserted++;
		}
		else if (point_in_buf(origPoints, px, py, d2)) {
		    j++;
		    if (inserted)
			close_loop = 0;
		}
		else {
		    break;
		}
	    }
	    else {
		break;
	    }
	}
	if (j > 0) {
	    Points->n_points -= j;
	}

	/* remove points from start in buffer */
	j = inserted = 0;
	for (i = 0; i < Points->n_points - 1; i++) {
	    x = Points->x;
	    y = Points->y;
	    px = (x[i] + x[i + 1]) / 2;
	    py = (y[i] + y[i + 1]) / 2;
	    if (point_in_buf(origPoints, x[i], y[i], d2)) {
		
		/* check for intersection of this segment */
		/* must not be the first or last point of this segment */
		current = i;
		if (find_cross_from_start(Points, current, &sa, &ix, &iy) != 0) {
		    /* insert intersection point into this segment */
		    /* and adjust i and Points->n_points */
		    int k = 0;
		    
		    if ((ix == x[sa] && iy == y[sa]) ||
		        (ix == x[sa + 1] && iy == y[sa + 1])) {
			    
			/* do not insert ix,iy into sa segment */
			Vect_append_point(Points, Points->x[Points->n_points - 1],
					  Points->y[Points->n_points - 1], 0);
			
			for (k = Points->n_points - 2; k > current + 1; k--) {
			    Points->x[k] = Points->x[k - 1];
			    Points->y[k] = Points->y[k - 1];
			}
			Points->x[current + 1] = ix;
			Points->y[current + 1] = iy;
			i--;
		    }
		    else {
			Vect_append_point(Points, Points->x[Points->n_points - 1],
					  Points->y[Points->n_points - 1], 0);
			Vect_append_point(Points, Points->x[Points->n_points - 1],
					  Points->y[Points->n_points - 1], 0);
			
			for (k = Points->n_points - 2; k > sa + 2; k--) {
			    Points->x[k] = Points->x[k - 2];
			    Points->y[k] = Points->y[k - 2];
			}
			Points->x[sa + 2] = ix;
			Points->y[sa + 2] = iy;
			for (k = sa + 1; k > current + 1; k--) {
			    Points->x[k] = Points->x[k - 1];
			    Points->y[k] = Points->y[k - 1];
			}
			Points->x[current + 1] = ix;
			Points->y[current + 1] = iy;
			i--;
		    }
		    inserted++;
		}
		else if (point_in_buf(origPoints, px, py, d2)) {
		    j++;
		    if (inserted)
			close_loop = 0;
		}
		else {
		    break;
		}
	    }
	    else {
		break;
	    }
	}
	x = Points->x;
	y = Points->y;
	if (j > 0) {
	    npn = 0;
	    for (i = j; i < Points->n_points; i++) {
		x[npn] = x[i];
		y[npn] = y[i];
		npn++;
	    }
	    Points->n_points = npn;
	}

	/* test for intersection of end segments */
	/* need to do this here, otherwise remove loops below will
	 * remove everything but the end dangles */
	np = Points->n_points;
	if (np > 3 && (x[0] != x[np - 1] || y[0] != y[np - 1])) {
	    i = 0;
	    j = np - 2;
	    ret =
		dig_test_for_intersection(x[i], y[i], x[i + 1], y[i + 1],
					  x[j], y[j], x[j + 1], y[j + 1]);
					  
	    if (ret == -1) {
		/* overlap */
		x[np - 1] = x[0];
		y[np - 1] = y[0];
	    }
	    else if (ret == 1) {
		dig_find_intersection(x[i], y[i], x[i + 1], y[i + 1], x[j],
				      y[j], x[j + 1], y[j + 1], &ix, &iy);
		x[0] = ix;
		y[0] = iy;
		x[np - 1] = ix;
		y[np - 1] = iy;
	    }
	}
	Vect_line_prune(Points);
    }

    if (sPoints == NULL)
	sPoints = Vect_new_line_struct();

    Vect_reset_line(sPoints);

    np = Points->n_points;
    npn = 1;

    side = (int)(d / fabs(d));

    /* remove loops */
    while (np > 3 && first < np - 2) {
	/* find first loop which doesn't contain any other loop */
	current = first;
	G_debug(5, "current: %d", current);
	last = Points->n_points - 2;
	lcount = 0;
	sa = current;
	while (find_cross
	       (Points, current, last - 1, current + 1, last, &sa, &sb) != 0) {

	    if (lcount == 0)
		first = sa;  /* move first forward */

	    current = sa + 1;
	    last = sb;
	    lcount++;
	    G_debug(5, "  current = %d, last = %d, lcount = %d", current,
		    last, lcount);
	}
	if (lcount == 0) {
	    break;
	}			/* loop not found */

	/* ensure sa is monotonically increasing, so npn doesn't reset low */
	/* disabled, otherwise nested loops are missed
	if (sa > sa_max)
	    sa_max = sa;
	if (sa < sa_max)
	    break;
	*/
	G_debug(4, "sa: %d", sa);

	/* remove loop if in buffer */
	if ((sb - sa) == 1) {	/* neighbouring lines overlap */
	    j = sb + 1;
	    npn = sa + 1;
	    /* first = sa; */
	}
	else {
	    double area_size;
	    int in_buffer = 0;

	    Vect_reset_line(sPoints);
	    dig_find_intersection(x[sa], y[sa], x[sa + 1], y[sa + 1], x[sb],
				  y[sb], x[sb + 1], y[sb + 1], &ix, &iy);

	    if (ix == x[0] && iy == y[0] && ix == x[np - 1] && iy == y[np - 1])
		break;

	    Vect_append_point(sPoints, ix, iy, 0);
	    for (i = sa + 1; i < sb + 1; i++) {	/* create loop polygon */
		Vect_append_point(sPoints, x[i], y[i], 0);
	    }
	    /* close polygon */
	    Vect_append_point(sPoints, ix, iy, 0);
	    Vect_line_prune(sPoints);

	    /* is loop in buffer ? */
	    dig_find_area_poly(sPoints, &area_size);
	    in_buffer = area_size * side < 0;
	    if (!in_buffer && area_size) {
		/* Vect_find_poly_centroid() may produce coords outside polygon */
		Vect_find_poly_centroid(sPoints, &px, &py);
		in_buffer = point_in_buf(origPoints, px, py, d2) != 0;
	    }

	    if (in_buffer) {
		npn = sa + 1;
		/* do not create zero-length segments */
		if (ix != x[sa] && iy != y[sa]) {
		    x[npn] = ix;
		    y[npn] = iy;
		    npn++;
		}
		if (ix != x[sb + 1] && iy != y[sb + 1]) {
		    j = sb + 1;
		}
		else {
		    j = sb + 2;
		}
		
		/* lcount == 0 can not happen
		if (lcount == 0) {
		    first = sa;
		}
		*/
	    }
	    else {		/* loop is not in buffer */
		first = sb;
		continue;
	    }
	}

	for (i = j; i < Points->n_points; i++) {	/* move points down */
	    x[npn] = x[i];
	    y[npn] = y[i];
	    npn++;
	}
	Points->n_points = np = npn;
    }

    Vect_line_prune(Points);

    /* looped input line ? */
    if (origPoints->x[0] == origPoints->x[origPoints->n_points - 1] &&
        origPoints->y[0] == origPoints->y[origPoints->n_points - 1] &&
	close_loop) {
	double tx, ty, vx, vy, ux, uy, wx, wy, a, aw, av;
	int sa, sb, side;
	double atol, atol2;
	double *xorg, *yorg;
	
	atol = 2 * acos(1 - tol / fabs(d));

	side = (int)(d / fabs(d));

	xorg = origPoints->x;
	yorg = origPoints->y;

	np = origPoints->n_points;

	/* last segment */
	vect(xorg[np - 2], yorg[np - 2], xorg[np - 1], yorg[np - 1], &tx, &ty);
	vx = ty * d;
	vy = -tx * d;
	/* first segment */
	vect(xorg[0], yorg[0], xorg[1], yorg[1], &ux, &uy);
	wx = uy * d;
	wy = -ux * d;
	av = atan2(vy, vx);
	aw = atan2(wy, wx);
	a = (aw - av) * side;
	if (a < 0)
	    a += 2 * PI;
	    
	if (a > PI) {
	    G_debug(5, "search intersection");
	    /* there can be two intersections !!! */
	    /* a > PI is probably handled by remove ends above */
	    if (find_cross_reverse(Points, 0, Points->n_points - 2, 1, Points->n_points - 1, &sa,
		&sb) != 0) {
		G_debug(5, "found intersection");
		dig_find_intersection(x[sa], y[sa], x[sa + 1], y[sa + 1], x[sb],
				      y[sb], x[sb + 1], y[sb + 1], &ix, &iy);
		x[0] = ix;
		y[0] = iy;
		npn = 1;
		/* move points down */
		for (i = sa + 1; i <= sb; i++) {
		    x[npn] = x[i];
		    y[npn] = y[i];
		    npn++;
		}
		Points->n_points = npn;
	    }
	}
	/* TODO: a <= PI can probably fail because of representation error */
	else {
	    if (a <= PI && a > atol) {
		int na;
		double nx, ny;

		/* OK to close parallel line ? */
		na = (int)(a / atol);
		atol2 = a / (na + 1) * side;
		for (j = 0; j < na; j++) {
		    av += atol2;
		    nx = xorg[0] + fabs(d) * cos(av);
		    ny = yorg[0] + fabs(d) * sin(av);
		    Vect_append_point(Points, nx, ny, 0);
		}
	    }
	}
	/* always close for looped input line */
	Vect_append_point(Points, Points->x[0], Points->y[0], 0);

	Vect_line_prune(Points);
    }

}

/* parallel_line - remove duplicate points from input line and
 *  creates new parallel line in 'd' offset distance;
 *  'tol' is tolerance between arc and polyline;
 *  this function doesn't care about created loops;
 *
 *  New line is written to existing nPoints structure.
 */
static void parallel_line(struct line_pnts *Points, double d, double tol,
			  struct line_pnts *nPoints)
{
    int i, j, np, na, side;
    double *x, *y, nx, ny, tx, ty, vx, vy, ux, uy, wx, wy;
    double atol, atol2, a, av, aw;

    G_debug(4, "parallel_line()");

    Vect_reset_line(nPoints);

    np = Points->n_points;
    x = Points->x;
    y = Points->y;

    if (np == 0)
	return;

    if (np == 1) {
	Vect_append_point(nPoints, x[0], y[0], 0);	/* ? OK, should make circle for points ? */
	return;
    }

    if (d == 0) {
	Vect_copy_xyz_to_pnts(nPoints, x, y, NULL, np);
	return;
    }

    side = (int)(d / fabs(d));
    atol = 2 * acos(1 - tol / fabs(d));

    for (i = 0; i < np - 1; i++) {
	vect(x[i], y[i], x[i + 1], y[i + 1], &tx, &ty);
	vx = ty * d;
	vy = -tx * d;

	nx = x[i] + vx;
	ny = y[i] + vy;
	Vect_append_point(nPoints, nx, ny, 0);

	nx = x[i + 1] + vx;
	ny = y[i + 1] + vy;
	Vect_append_point(nPoints, nx, ny, 0);

	if (i < np - 2) {	/* use polyline instead of arc between line segments */
	    vect(x[i + 1], y[i + 1], x[i + 2], y[i + 2], &ux, &uy);
	    wx = uy * d;
	    wy = -ux * d;
	    av = atan2(vy, vx);
	    aw = atan2(wy, wx);
	    a = (aw - av) * side;
	    if (a < 0)
		a += 2 * PI;

	    /* TODO: a <= PI can probably fail because of representation error */
	    if (a <= PI && a > atol) {
		na = (int)(a / atol);
		atol2 = a / (na + 1) * side;
		for (j = 0; j < na; j++) {
		    av += atol2;
		    nx = x[i + 1] + fabs(d) * cos(av);
		    ny = y[i + 1] + fabs(d) * sin(av);
		    Vect_append_point(nPoints, nx, ny, 0);
		}
	    }
	}
    }
    Vect_line_prune(nPoints);
}

/*!
   \brief Create parrallel line

   \param InPoints input line
   \param distance create parrallel line in distance
   \param tolerance maximum distance between theoretical arc and polygon segments
   \param rm_end remove end points falling into distance
   \param[out] OutPoints output line

   \return
 */
void
Vect_line_parallel(struct line_pnts *InPoints, double distance,
		   double tolerance, int rm_end, struct line_pnts *OutPoints)
{
    G_debug(4,
	    "Vect_line_parallel(): npoints = %d, distance = %f, tolerance = %f",
	    InPoints->n_points, distance, tolerance);

    parallel_line(InPoints, distance, tolerance, OutPoints);
    G_debug(4, "%d outpoints", OutPoints->n_points);

    clean_parallel(OutPoints, InPoints, distance, tolerance, rm_end, 1);
    G_debug(4, "%d outpoints after cleaning", OutPoints->n_points);

    return;
}

/*!
   \brief Create buffer around the line line.

   Buffer is closed counter clockwise polygon.
   Warning: output line may contain loops!

   \param InPoints input line
   \param distance create buffer in distance
   \param tolerance maximum distance between theoretical arc and polygon segments
   \param[out] OutPoints output line
 */
void
Vect_line_buffer(struct line_pnts *InPoints, double distance,
		 double tolerance, struct line_pnts *OutPoints)
{
    double dangle;
    int side, npoints;
    static struct line_pnts *Points = NULL;
    static struct line_pnts *PPoints = NULL;
    double d2 = distance * distance * D_MULT;

    distance = fabs(distance);

    dangle = 2 * acos(1 - tolerance / fabs(distance));	/* angle step */

    if (Points == NULL)
	Points = Vect_new_line_struct();

    if (PPoints == NULL)
	PPoints = Vect_new_line_struct();

    /* Copy input */
    Vect_reset_line(Points);
    Vect_append_points(Points, InPoints, GV_FORWARD);

    Vect_reset_line(OutPoints);

    npoints = Points->n_points;
    if (npoints <= 0) {
	return;
    }
    else if (npoints == 1) {	/* make a circle */
	double angle, x, y;

	for (angle = 0; angle < 2 * PI; angle += dangle) {
	    x = Points->x[0] + distance * cos(angle);
	    y = Points->y[0] + distance * sin(angle);
	    Vect_append_point(OutPoints, x, y, 0);
	}
	/* Close polygon */
	Vect_append_point(OutPoints, OutPoints->x[0], OutPoints->y[0], 0);
    }
    else {			/* 2 and more points */
	for (side = 0; side < 2; side++) {
	    double angle, sangle;
	    double lx1, ly1, lx2, ly2;
	    double x, y, nx, ny, sx, sy, ex, ey;

	    /* Parallel on one side */
	    if (side == 0) {
		/* Vect_line_parallel(Points, distance, tolerance, 0, PPoints); */

		/* just create parallel line, clean later */
		parallel_line(InPoints, distance, tolerance, PPoints);
		G_debug(4, "%d outpoints", PPoints->n_points);

		Vect_append_points(OutPoints, PPoints, GV_FORWARD);
	    }
	    else {
		/* Vect_line_parallel(Points, -distance, tolerance, 0, PPoints); */

		parallel_line(InPoints, -distance, tolerance, PPoints);
		G_debug(4, "%d outpoints", PPoints->n_points);

		Vect_append_points(OutPoints, PPoints, GV_BACKWARD);
	    }

	    /* Arc at the end */
	    /* 2 points at the end of original line */
	    if (side == 0) {
		lx1 = Points->x[npoints - 2];
		ly1 = Points->y[npoints - 2];
		lx2 = Points->x[npoints - 1];
		ly2 = Points->y[npoints - 1];
	    }
	    else {
		lx1 = Points->x[1];
		ly1 = Points->y[1];
		lx2 = Points->x[0];
		ly2 = Points->y[0];
	    }

	    /* normalized vector */
	    vect(lx1, ly1, lx2, ly2, &nx, &ny);

	    /* starting point */
	    sangle = atan2(-nx, ny);	/* starting angle */
	    sx = lx2 + ny * distance;
	    sy = ly2 - nx * distance;

	    /* end point */
	    ex = lx2 - ny * distance;
	    ey = ly2 + nx * distance;

	    Vect_append_point(OutPoints, sx, sy, 0);

	    /* arc */
	    for (angle = dangle; angle < PI; angle += dangle) {
		x = lx2 + distance * cos(sangle + angle);
		y = ly2 + distance * sin(sangle + angle);
		
		Vect_append_point(OutPoints, x, y, 0);
	    }

	    Vect_append_point(OutPoints, ex, ey, 0);
	}

	/* clean up arcs at the end of input lines */
	Vect_line_prune(OutPoints);
	if (OutPoints->x[0] == OutPoints->x[OutPoints->n_points - 1] &&
	    OutPoints->y[0] == OutPoints->y[OutPoints->n_points - 1])
	    OutPoints->n_points--;

	clean_parallel(OutPoints, InPoints, distance, tolerance, 2, 0);

	/* Close polygon */
	if (OutPoints->x[0] != OutPoints->x[OutPoints->n_points - 1] ||
	    OutPoints->y[0] != OutPoints->y[OutPoints->n_points - 1]) {

	    if (point_in_buf(InPoints, OutPoints->x[0], OutPoints->y[0], d2)) {
		OutPoints->x[0] = OutPoints->x[OutPoints->n_points - 1];
		OutPoints->y[0] = OutPoints->y[OutPoints->n_points - 1];
	    }
	    else if (point_in_buf(InPoints, OutPoints->x[OutPoints->n_points - 1],
				  OutPoints->y[OutPoints->n_points - 1], d2)) {
		OutPoints->x[OutPoints->n_points - 1] = OutPoints->x[0];
		OutPoints->y[OutPoints->n_points - 1] = OutPoints->y[0];
	    }
	    else
		Vect_append_point(OutPoints, OutPoints->x[0], OutPoints->y[0], 0);
	}
    }

    return;
}
