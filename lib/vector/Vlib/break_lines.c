/*!
 * \file break_lines.c
 *
 * \brief Vector library - Clean vector map (break lines)
 *
 * (C) 2001-2008 by the GRASS Development Team
 *
 * This program is free software under the 
 * GNU General Public License (>=v2). 
 * Read the file COPYING that comes with GRASS
 * for details.
 *
 * \author Radim Blazek
 *
 * \date 2001
 */

#include <stdlib.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/glocale.h>

/*!
   \brief Break lines in vector map at each intersection.

   For details see Vect_break_lines_list().

   \param Map input vector map 
   \param type feature type
   \param[out] Err vector map where points at intersections will be written or NULL

   \return
 */

void
Vect_break_lines(struct Map_info *Map, int type, struct Map_info *Err)
{
    Vect_break_lines_list(Map, NULL, NULL, type, Err);

    return;
}

/*!
   \brief Break selected lines in vector map at each intersection.

   Breaks selected lines specified by type in vector map. Points at
   intersections may be optionally written to error map. Input vector map
   must be opened on level 2 for update at least on GV_BUILD_BASE.

   The function also breaks lines forming collapsed loop, for example
   0,0;1,0;0,0 is broken at 1,0.

   If reference lines are given (<i>List_ref</i>) break only lines
   which intersect reference lines.

   \param Map input vector map 
   \param List_break list of lines (NULL for all lines in vector map)
   \param List_ref list of reference lines or NULL
   \param type feature type
   \param[out] Err vector map where points at intersections will be written or NULL

   \return number of intersections
 */

int
Vect_break_lines_list(struct Map_info *Map, struct ilist *List_break,
		      struct ilist *List_ref, int type, struct Map_info *Err)
{
    struct line_pnts *APoints, *BPoints, *Points;
    struct line_pnts **AXLines, **BXLines;
    struct line_cats *ACats, *BCats, *Cats;
    int j, k, l, ret, atype, btype, aline, bline, found, iline, nlines;
    int naxlines, nbxlines, nx;
    double *xx = NULL, *yx = NULL, *zx = NULL;
    BOUND_BOX ABox, BBox;
    struct ilist *List;
    int nbreaks;
    int touch1_n = 0, touch1_s = 0, touch1_e = 0, touch1_w = 0;	/* other vertices except node1 touching box */
    int touch2_n = 0, touch2_s = 0, touch2_e = 0, touch2_w = 0;	/* other vertices except node2 touching box */
    int is3d;
    int node, anode1, anode2, bnode1, bnode2;
    double nodex, nodey;

    APoints = Vect_new_line_struct();
    BPoints = Vect_new_line_struct();
    Points = Vect_new_line_struct();
    ACats = Vect_new_cats_struct();
    BCats = Vect_new_cats_struct();
    Cats = Vect_new_cats_struct();
    List = Vect_new_list();

    is3d = Vect_is_3d(Map);

    if (List_break) {
	nlines = List_break->n_values;
    }
    else {
	nlines = Vect_get_num_lines(Map);
    }
    G_debug(3, "nlines =  %d", nlines);

    /* To find intersection of two lines (Vect_line_intersection) is quite slow.
     * Fortunately usual lines/boundaries in GIS often forms a network where lines
     * are connected by end points, and touch by MBR. This function checks and occasionaly
     * skips such cases. This is currently done for 2D only
     */

    /* Go through all lines in vector, for each select lines which overlap MBR of
     * this line exclude those connected by one endpoint (see above)
     * and try to intersect, if lines intersect write new lines at the end of 
     * the file, and process next line (remaining lines overlapping box are skipped)
     */
    nbreaks = 0;

    for (iline = 0; iline < nlines; iline++) {
	G_percent(iline, nlines, 1);
	if (List_break) {
	    aline = List_break->value[iline];
	}
	else {
	    aline = iline + 1;
	}

	if (List_ref && !Vect_val_in_list(List_ref, aline))
	    continue;

	G_debug(3, "aline =  %d", aline);
	if (!Vect_line_alive(Map, aline))
	    continue;

	atype = Vect_read_line(Map, APoints, ACats, aline);
	if (!(atype & type))
	    continue;

	Vect_get_line_box(Map, aline, &ABox);

	/* Find which sides of the box are touched by intermediate (non-end) points of line */
	if (!is3d) {
	    touch1_n = touch1_s = touch1_e = touch1_w = 0;
	    for (j = 1; j < APoints->n_points; j++) {
		if (APoints->y[j] == ABox.N)
		    touch1_n = 1;
		if (APoints->y[j] == ABox.S)
		    touch1_s = 1;
		if (APoints->x[j] == ABox.E)
		    touch1_e = 1;
		if (APoints->x[j] == ABox.W)
		    touch1_w = 1;
	    }
	    G_debug(3, "touch1: n = %d s = %d e = %d w = %d", touch1_n,
		    touch1_s, touch1_e, touch1_w);
	    touch2_n = touch2_s = touch2_e = touch2_w = 0;
	    for (j = 0; j < APoints->n_points - 1; j++) {
		if (APoints->y[j] == ABox.N)
		    touch2_n = 1;
		if (APoints->y[j] == ABox.S)
		    touch2_s = 1;
		if (APoints->x[j] == ABox.E)
		    touch2_e = 1;
		if (APoints->x[j] == ABox.W)
		    touch2_w = 1;
	    }
	    G_debug(3, "touch2: n = %d s = %d e = %d w = %d", touch2_n,
		    touch2_s, touch2_e, touch2_w);
	}

	Vect_select_lines_by_box(Map, &ABox, type, List);
	G_debug(3, "  %d lines selected by box", List->n_values);

	for (j = 0; j < List->n_values; j++) {
	    bline = List->value[j];
	    if (List_break && !Vect_val_in_list(List_break, bline)) {
		continue;
	    }
	    G_debug(3, "  j = %d bline = %d", j, bline);

	    /* Check if thouch by end node only */
	    if (!is3d) {
		Vect_get_line_nodes(Map, aline, &anode1, &anode2);
		Vect_get_line_nodes(Map, bline, &bnode1, &bnode2);
		Vect_get_line_box(Map, bline, &BBox);

		if (anode1 == bnode1 || anode1 == bnode2)
		    node = anode1;
		else if (anode2 == bnode1 || anode2 == bnode2)
		    node = anode2;
		else
		    node = 0;

		if (node) {
		    Vect_get_node_coor(Map, node, &nodex, &nodey, NULL);
		    if ((node == anode1 && nodey == ABox.N && !touch1_n &&
			 nodey == BBox.S) || (node == anode2 &&
					      nodey == ABox.N && !touch2_n &&
					      nodey == BBox.S) ||
			(node == anode1 && nodey == ABox.S && !touch1_s &&
			 nodey == BBox.N) || (node == anode2 &&
					      nodey == ABox.S && !touch2_s &&
					      nodey == BBox.N) ||
			(node == anode1 && nodex == ABox.E && !touch1_e &&
			 nodex == BBox.W) || (node == anode2 &&
					      nodex == ABox.E && !touch2_e &&
					      nodex == BBox.W) ||
			(node == anode1 && nodex == ABox.W && !touch1_w &&
			 nodex == BBox.E) || (node == anode2 &&
					      nodex == ABox.W && !touch2_w &&
					      nodex == BBox.E)) {
			G_debug(3,
				"lines %d and %d touching by end nodes only -> no intersection",
				aline, bline);
			continue;
		    }
		}
	    }

	    btype = Vect_read_line(Map, BPoints, BCats, bline);

	    AXLines = NULL;
	    BXLines = NULL;
	    Vect_line_intersection(APoints, BPoints, &AXLines, &BXLines,
				   &naxlines, &nbxlines, 0);
	    G_debug(3, "  naxlines = %d nbxlines = %d", naxlines, nbxlines);

	    /* This part handles a special case when aline == bline, no other intersection was found
	     * and the line is forming collapsed loop, for example  0,0;1,0;0,0 should be broken at 1,0.
	     * ---> */
	    if (aline == bline && naxlines == 0 && nbxlines == 0 &&
		APoints->n_points >= 3) {
		int centre;
		int i;

		G_debug(3, "  Check collapsed loop");
		if (APoints->n_points % 2) {	/* odd number of vertices */
		    centre = APoints->n_points / 2;	/* index of centre */
		    if (APoints->x[centre - 1] == APoints->x[centre + 1] && APoints->y[centre - 1] == APoints->y[centre + 1] && APoints->z[centre - 1] == APoints->z[centre + 1]) {	/* -> break */
			AXLines =
			    (struct line_pnts **)G_malloc(2 *
							  sizeof(struct
								 line_pnts
								 *));
			AXLines[0] = Vect_new_line_struct();
			AXLines[1] = Vect_new_line_struct();

			for (i = 0; i <= centre; i++)
			    Vect_append_point(AXLines[0], APoints->x[i],
					      APoints->y[i], APoints->z[i]);

			for (i = centre; i < APoints->n_points; i++)
			    Vect_append_point(AXLines[1], APoints->x[i],
					      APoints->y[i], APoints->z[i]);

			naxlines = 2;
		    }
		}
	    }
	    /* <--- */

	    if (Err) {		/* array for intersections (more than needed */
		xx = (double *)G_malloc((naxlines + nbxlines) *
					sizeof(double));
		yx = (double *)G_malloc((naxlines + nbxlines) *
					sizeof(double));
		zx = (double *)G_malloc((naxlines + nbxlines) *
					sizeof(double));
	    }
	    nx = 0;		/* number of intersections to be written to Err */
	    if (naxlines > 0) {	/* intersection -> write out */
		Vect_delete_line(Map, aline);
		for (k = 0; k < naxlines; k++) {
		    /* Write new line segments */
		    /* line may collapse, don't write zero length lines */
		    Vect_line_prune(AXLines[k]);
		    if ((atype & GV_POINTS) || AXLines[k]->n_points > 1) {
			ret = Vect_write_line(Map, atype, AXLines[k], ACats);
			if (List_ref) {
			    Vect_list_append(List_ref, ret);
			}
			G_debug(3, "Line %d written, npoints = %d", ret,
				AXLines[k]->n_points);
			if (List_break) {
			    Vect_list_append(List_break, ret);
			}
		    }

		    /* Write intersection points */
		    if (Err) {
			if (k > 0) {
			    xx[nx] = AXLines[k]->x[0];
			    yx[nx] = AXLines[k]->y[0];
			    zx[nx] = AXLines[k]->z[0];
			    nx++;
			}
		    }
		    Vect_destroy_line_struct(AXLines[k]);
		}
		nbreaks += naxlines - 1;
	    }
	    if (AXLines)
		G_free(AXLines);

	    if (nbxlines > 0) {
		if (aline != bline) {	/* Self intersection, do not write twice, TODO: is it OK? */
		    Vect_delete_line(Map, bline);
		    for (k = 0; k < nbxlines; k++) {
			/* Write new line segments */
			/* line may collapse, don't write zero length lines */
			Vect_line_prune(BXLines[k]);
			if ((btype & GV_POINTS) || BXLines[k]->n_points > 1) {
			    ret =
				Vect_write_line(Map, btype, BXLines[k],
						BCats);
			    G_debug(5, "Line %d written", ret);
			    if (List_break) {
				Vect_list_append(List_break, ret);
			    }
			}

			/* Write intersection points */
			if (Err) {
			    if (k > 0) {
				found = 0;
				for (l = 0; l < nx; l++) {
				    if (xx[l] == BXLines[k]->x[0] &&
					yx[l] == BXLines[k]->y[0] &&
					zx[l] == BXLines[k]->z[0]) {
					found = 1;
					break;
				    }
				}
				if (!found) {
				    xx[nx] = BXLines[k]->x[0];
				    yx[nx] = BXLines[k]->y[0];
				    zx[nx] = BXLines[k]->z[0];
				    nx++;
				}
			    }
			}
			Vect_destroy_line_struct(BXLines[k]);
		    }
		    nbreaks += nbxlines - 1;
		}
		else {
		    for (k = 0; k < nbxlines; k++)
			Vect_destroy_line_struct(BXLines[k]);
		}
	    }
	    if (BXLines)
		G_free(BXLines);
	    if (Err) {
		for (l = 0; l < nx; l++) {	/* Write out errors */
		    Vect_reset_line(Points);
		    Vect_append_point(Points, xx[l], yx[l], zx[l]);
		    ret = Vect_write_line(Err, GV_POINT, Points, Cats);
		}

		G_free(xx);
		G_free(yx);
		G_free(zx);
	    }
	    if (naxlines > 0)
		break;		/* first line was broken and deleted -> take the next one */
	}

	if (List_break) {
	    nlines = List_break->n_values;
	}
	else {
	    nlines = Vect_get_num_lines(Map);
	}
	G_debug(3, "nlines =  %d", nlines);
    }				/* for each line */
    G_percent(nlines, nlines, 1); /* finish it */

    G_verbose_message(_("Intersections: %5d"), nbreaks);

    Vect_destroy_list(List);

    return nbreaks;
}
