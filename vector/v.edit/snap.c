/****************************************************************
 *
 * MODULE:     v.edit
 *
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym  les-ejk cz>
 *             Martin Landa <landa.martin gmail.com>
 *
 * PURPOSE:    This module edits vector map, snapping section.
 *
 * COPYRIGHT:  (C) 2007 by the GRASS Development Team
 *
 *             This program is free software under the
 *             GNU General Public License (>=v2).
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 ****************************************************************/

#include "global.h"

/**
   \brief Global snapping function based on snapping library function.

   \param[in] Map vector map
   \param[in] List list of lines to be snapped
   \param[in] thresh threshold distance for snapping

   \return 1
 */
int do_snap(struct Map_info *Map,
	    struct ilist *List, double thresh) {
    
    FILE * output;

    if (G_verbose() > G_verbose_min()) {
	G_important_message (SEP);
	output = stderr;
    }
    else
	output = NULL;

    Vect_snap_lines_list (Map, List, thresh, NULL, output);

    if (G_verbose() > G_verbose_min()) {
	G_important_message (SEP);
    }

    return 1;
}

/**
   \brief Snap two selected lines

   \param[in] Map vector map
   \param[in] line1 reference line
   \param[in] line2 line to be snapped (to be modified)
   \param[in] thresh threshold distance for snapping (-1 for no limit)

   \return id of snapped line
   \return 0 lines not snapped
   \return -1 on error
*/
int do_snap_line(struct Map_info *Map,
		 int line1, int line2, double thresh)
{
    struct line_pnts *Points1, *Points2;
    struct line_cats *Cats2;
    int type1, type2;
    int newline;
    double mindist;
    int mindistidx;
    
    Points1 = Vect_new_line_struct();
    Points2 = Vect_new_line_struct();
    Cats2 = Vect_new_cats_struct();

    type1 = Vect_read_line(Map, Points1, NULL, line1);
    type2 = Vect_read_line(Map, Points2, Cats2, line2);

    /* find mininal distance and its indexes */
    mindist = min_distance_line (Points1, Points2,
				 &mindistidx);

    if (thresh > 0.0 && mindist > thresh) {
	Vect_destroy_line_struct(Points1);
	Vect_destroy_line_struct(Points2);
	Vect_destroy_cats_struct(Cats2);
	return 0;
    }

    switch(mindistidx) {
    case 0: 
	Points2->x[0] = Points1->x[0];
	Points2->y[0] = Points1->y[0];
	Points2->z[0] = Points1->z[0];
	break;
    case 1: 
	Points2->x[Points2->n_points-1] = Points1->x[0];
	Points2->y[Points2->n_points-1] = Points1->y[0];
	Points2->z[Points2->n_points-1] = Points1->z[0];
	break;
    case 2: 
	Points2->x[0] = Points1->x[Points1->n_points-1];
	Points2->y[0] = Points1->y[Points1->n_points-1];
	Points2->z[0] = Points1->z[Points1->n_points-1];
	break;
    case 3: 
	Points2->x[Points2->n_points-1] = Points1->x[Points1->n_points-1];
	Points2->y[Points2->n_points-1] = Points1->y[Points1->n_points-1];
	Points2->z[Points2->n_points-1] = Points1->z[Points1->n_points-1];
	break;
    default:
	break;
    }

    newline = Vect_rewrite_line (Map, line2, type2, Points2, Cats2);
    if (newline < 0) {
	G_warning(_("Unable to rewrite line %d"), line2);
        return -1;
    }

    /*
    G_message(_("Line %d snapped to line %d"),
	      line2, line1);
    */
    Vect_destroy_line_struct(Points1);
    Vect_destroy_line_struct(Points2);
    Vect_destroy_cats_struct(Cats2);
    
    return newline;
}

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
int do_snap_point(struct Map_info *Map,
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
	}
    }

    Vect_destroy_line_struct(Points);

    return snapped;
}

/**
   \brief Snap lines/boudaries to the nearest feature
   
   \param[in] Map vector map
   \param[in] BgMap,nbgmaps List of background maps
   \param[in] List list of lines to be snapped
   \param[in] layer layer number
   \param[in] thresh threshold value used for snapping
   \param[in] to_vertex allow snapping also to vertex

   \return number of snapped lines
*/
int do_snapping(struct Map_info *Map, struct Map_info **BgMap, int nbgmaps,
		struct ilist* List,
		double thresh, int to_vertex)
{
    int i, line, type, npoints, node, rewrite;
    int nlines_modified;
    double *x, *y, *z;

    struct line_pnts *Points;
    struct line_cats *Cats;

    nlines_modified = 0;

    Points = Vect_new_line_struct();
    Cats   = Vect_new_cats_struct();

    for(i = 0; i < List -> n_values; i++) {
	line = List -> value[i];

	if (!Vect_line_alive (Map, line))
	    continue;

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

	    if (do_snap_point(Map, line, &x[node], &y[node], &z[node], thresh,
			      to_vertex)) {
		rewrite = 1;
	    }
	    else {
	      /* check also background maps */
	      int bgi;
	      for (bgi = 0; bgi < nbgmaps; bgi++) {
		if (do_snap_point(BgMap[i], line, &x[node], &y[node], &z[node], thresh,
				  to_vertex)) {
		  break; /* snapped, don't continue */
		  rewrite = 1;
		}
	      }
	    }
	} /* for each point */
	
	if (rewrite) {
	    if (Vect_rewrite_line (Map, line, type, Points, Cats) < 0) {
		G_warning(_("Unable to rewrite line %d"), line);
		return -1;
	    }
	    nlines_modified++;
	}
    } /* for each line */

    Vect_destroy_line_struct(Points);
    Vect_destroy_cats_struct(Cats);

    return nlines_modified;
}
