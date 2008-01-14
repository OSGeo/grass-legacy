/**
   \brief Vedit library - snapping

   This program is free software under the
   GNU General Public License (>=v2).
   Read the file COPYING that comes with GRASS
   for details.

   \author (C) 2007-2008 by the GRASS Development Team
   Martin Landa <landa.martin gmail.com>
   Jachym Cepicky <jachym.cepicky gmail.com>

   \date 2007-2008
*/

#include "vedit.h"

/**
   \brief Move selected features
   
   \param[in] Map vector map
   \param[in] List list of features to be moved
   \param[in] move_x,move_y,move_z direction (move_z used only if map is 3D)
   \param[in] snap enable snapping (see globals.h)

   \return number of modified features
   \return -1 on error
*/
int Vedit_move_lines(struct Map_info *Map, struct ilist *List, 
		     double move_x, double move_y, double move_z,
		     int snap, double thresh)
{
    struct line_pnts *Points;
    struct line_cats *Cats;
    int i, j;
    int type, newline, line;
    int nlines_moved;
    double *x, *y, *z;

    nlines_moved = 0;

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();

    for (i = 0; i < List->n_values; i++) {
	line = List -> value[i];

	if (!Vect_line_alive (Map, line))
	    continue;

        type = Vect_read_line(Map, Points, Cats, line);

        G_debug(3, "Vedit_move_lines(): type=%d, line=%d", type, line);

	x = Points->x;
	y = Points->y;
	z = Points->z;

        /* move */
        for (j = 0; j < Points -> n_points; j++) {
	    x[j] += move_x;
            y[j] += move_y;
	    if (Vect_is_3d(Map))
		z[j] += move_z;

	    if (snap != NO_SNAP) {
		if (Vedit_snap_point(Map, line, &x[j], &y[j], &z[j], thresh,
				     (snap == SNAPVERTEX) ? 1 : 0) == 0) {
		    /* check also background maps */
		}
	    }
        } /* for each point at line */

	newline = Vect_rewrite_line (Map, line, type, Points, Cats);

        if (newline < 0)  {
            return -1;
        }

        nlines_moved++;
    }
        
    Vect_destroy_line_struct (Points);
    Vect_destroy_cats_struct (Cats);

    return nlines_moved;
}
