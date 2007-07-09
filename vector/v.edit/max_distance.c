/****************************************************************
 *
 * MODULE:     v.edit
 *
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym  les-ejk cz>
 *             Martin Landa
 *
 * PURPOSE:    This module edits vector maps. 
 *
 * COPYRIGHT:  (C) 2002-2007 by the GRASS Development Team
 *
 *             This program is free software under the
 *             GNU General Public License (>=v2).
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 ****************************************************************/

#include "global.h"

/* 
 * set maxdistance based on the current resolution
 *
 * this code comes from v.what/main.c
 */
double max_distance(double maxdistance)
{
    struct Cell_head window;

    double ew_dist1, ew_dist2, ns_dist1, ns_dist2;
    double xres, yres, maxd;

    if (maxdistance < 0.0) {
	G_warning ("Threshold distance must be >= 0.0, using value 0.0");
    }

    if (maxdistance <= 0.0) {
        G_get_window (&window);

        ew_dist1 = G_distance(window.east, window.north, window.west, window.north);
        /* EW Dist at South Edge */
        ew_dist2 = G_distance(window.east, window.south, window.west, window.south);
        /* NS Dist at East edge */
        ns_dist1 = G_distance(window.east, window.north, window.east, window.south);
        /* NS Dist at West edge */
        ns_dist2 = G_distance(window.west, window.north, window.west, window.south);

        xres = ((ew_dist1 + ew_dist2) / 2) / window.cols;
        yres = ((ns_dist1 + ns_dist2) / 2) / window.rows;

        if (xres > yres)
	    maxd = xres;
	else
	    maxd = yres;

	G_message ("Threshold distance set to [%g] (based on 2D resolution)", maxd);
    }
    else {
        maxd = maxdistance;
    }

    G_debug (3, "max_distance(): threshold is %g", maxd);

    return maxd;
}

/* 
 * calculate distances between two lines
 *
 * array distances hold distances between first and last point of both lines:
 * distances[0] = first-first
 * distances[1] = first-last
 * distances[2] = last-first
 * distances[3] = last-last
 *
 * return minimal distance (its index stored in mindistidx variable)
 */
double min_distance_line (struct line_pnts *Points1, struct line_pnts *Points2,
			  int* mindistidx)
{
    unsigned int i;
    double distances [4];

    distances[0] = Vect_points_distance(Points1->x[0], Points1->y[0], Points1->z[0],
					Points2->x[0], Points2->y[0], Points2->z[0], 0);
    
    distances[1] = Vect_points_distance(Points1->x[0], Points1->y[0], Points1->z[0],
					Points2->x[Points2->n_points-1],
					Points2->y[Points2->n_points-1],
					Points2->z[Points2->n_points-1], 0);
    
    distances[2] = Vect_points_distance(Points1->x[Points1->n_points-1],
					Points1->y[Points1->n_points-1],
					Points1->z[Points1->n_points-1],
					Points2->x[0], Points2->y[0], Points2->z[0], 0);
    
    distances[3] = Vect_points_distance(Points1->x[Points1->n_points-1],
					Points1->y[Points1->n_points-1],
					Points1->z[Points1->n_points-1],
					Points2->x[Points2->n_points-1],
					Points2->y[Points2->n_points-1],
					Points2->z[Points2->n_points-1], 0);
    
    /* find the minimal distance between first or last point of both lines */
    *mindistidx = 0;
    for (i = 0; i < sizeof (distances) / sizeof (double); i++) {
	if (distances[i] > 0.0 && distances[i] < distances[*mindistidx])
	    *mindistidx = i;
    }

    return distances [*mindistidx];
}

/* 
 * creates bounding box (polygon) based on center point; size (2 * maxdist)
 */
void coord2bbox (double east, double north, double maxdist,
		struct line_pnts *box)
{
    /* TODO: 3D */
    Vect_reset_line(box);
    
    Vect_append_point(box, east - maxdist, north - maxdist, 0);
    Vect_append_point(box, east + maxdist, north - maxdist, 0);
    Vect_append_point(box, east + maxdist, north + maxdist, 0);
    Vect_append_point(box, east - maxdist, north + maxdist, 0);
    Vect_append_point(box, box->x[0], box->y[0], box->z[0]);
	
    return;
}
