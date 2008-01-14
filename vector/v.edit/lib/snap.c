/**
   \brief Vedit library - snapping

   This program is free software under the
   GNU General Public License (>=v2).
   Read the file COPYING that comes with GRASS
   for details.

   \author (C) 2007-2008 by the GRASS Development Team
   Martin Landa <landa.martin gmail.com>

   \date 2007-2008
*/

#include "vedit.h"

/**
   \brief Snap given point to the nearest feature
   
   \param[in] Map vector map
   \param[in] line line id to be snapped
   \param[in] x,y,z point on line to be snapped
   \param[in] thresh snapping threshold (>0)
   \param[in] vertex snap also to vertex

   \return 1 snapped
   \return 0 not snapped
*/
int Vedit_snap_point(struct Map_info *Map,
		     int line, double *x, double *y, double *z, double thresh,
		     int vertex)
{
    struct line_pnts *Points;
    
    int i, snapped;
    int line2snap, mindist_idx;
    double dist, mindist;
    
    snapped = 0;
    mindist_idx = -1;
    mindist = thresh;
    
    Points = Vect_new_line_struct();

    line2snap = Vect_find_line(Map, *x, *y, *z,
			       -1, thresh, WITHOUT_Z, line);
    
    if (line2snap > 0) {
	Vect_read_line(Map, Points, NULL, line2snap);
	
	if (!Vect_line_alive(Map, line2snap)) {
	    Vect_destroy_line_struct(Points);
	    return snapped;
	}

	for (i = 0; i < Points->n_points; i++) {
	    if (i > 0 && i < Points->n_points-1)
	      if (!vertex)
		continue;
	    dist = Vect_points_distance(*x, *y, *z,
					Points->x[i], Points->y[i], Points->z[i],
					WITHOUT_Z);

	    if (mindist >= dist) {
		mindist = dist;
		mindist_idx = i;
	    }
	}
				
	if(mindist_idx > -1) {
	    *x = Points->x[mindist_idx];
	    *y = Points->y[mindist_idx];
	    *z = Points->z[mindist_idx];
	    snapped = 1;
	    G_debug(3, "Vedit_snap_point(): line=%d", line2snap);
	}
    }

    Vect_destroy_line_struct(Points);

    return snapped;
}

/**
   \brief Snap lines/boudaries to the nearest feature
   
   \param[in] Map vector map
   \param[in] BgMap,nbgmaps List of background maps
   \param[in] line line to be snapped
   \param[in] layer layer number
   \param[in] thresh threshold value used for snapping (>0)
   \param[in] to_vertex allow snapping also to vertex

   \return 1 line snapped
   \return 0 line not snapped
   \return -1 line is dead
*/
int Vedit_snap_line(struct Map_info *Map, struct Map_info **BgMap, int nbgmaps,
		    int line,
		    double thresh, int to_vertex)
{
    int i, type, npoints, node, rewrite;
    double *x, *y, *z;

    struct line_pnts *Points;
    struct line_cats *Cats;

    Points = Vect_new_line_struct();
    Cats   = Vect_new_cats_struct();

    if (!Vect_line_alive (Map, line))
	return -1;

    type = Vect_read_line(Map, Points, Cats, line);
    npoints = Points -> n_points;
    x = Points->x;
    y = Points->y;
    z = Points->z;
    
    rewrite = 0;
    for (node = 0; node < npoints; node++) {
	if ((node > 0  && node < npoints -1) &&
	    !to_vertex)
	    continue;
	
	if (Vedit_snap_point(Map, line, &x[node], &y[node], &z[node], thresh,
			     to_vertex)) {
	    rewrite = 1;
	}
	else {
	    /* check also background maps */
	    int bgi;
	    for (bgi = 0; bgi < nbgmaps; bgi++) {
		if (Vedit_snap_point(BgMap[i], line, &x[node], &y[node], &z[node], thresh,
				     to_vertex)) {
		    rewrite = 1;
		    break; /* snapped, don't continue */
		}
	    }
	}
    } /* for each line vertex */
    
    /* close boundaries or lines */
    if (!rewrite && (type & GV_LINES) &&
	Vect_points_distance(x[0], y[0], z[0],
			     x[npoints-1], y[npoints-1], z[npoints-1],
			     WITHOUT_Z) <= thresh) {
	x[npoints-1] = x[0];
	y[npoints-1] = y[0];
	z[npoints-1] = z[0];
	
	rewrite = 1;
	G_debug(3, "Vedit_snap_line(): line=%d", line);
    }

    if (rewrite) {
	if (Vect_rewrite_line (Map, line, type, Points, Cats) < 0) {
	    return -1;
	}
    }
    
    Vect_destroy_line_struct(Points);
    Vect_destroy_cats_struct(Cats);

    return rewrite;
}

/**
   \brief Snap lines/boudaries to the nearest feature
   
   \param[in] Map vector map
   \param[in] BgMap,nbgmaps List of background maps
   \param[in] List list of lines to be snapped
   \param[in] layer layer number
   \param[in] thresh threshold value used for snapping (>0)
   \param[in] to_vertex allow snapping also to vertex

   \return number of snapped lines
*/
int Vedit_snap_lines(struct Map_info *Map, struct Map_info **BgMap, int nbgmaps,
		     struct ilist* List,
		     double thresh, int to_vertex)
{
    int i, line;
    int nlines_modified;

    for(i = 0; i < List -> n_values; i++) {
	line = List -> value[i];
	if (Vedit_snap_line(Map, BgMap, nbgmaps,
			    line, thresh, to_vertex) == 1) {
	    nlines_modified++;
	}
    }

    return nlines_modified;
}
