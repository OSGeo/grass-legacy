/****************************************************************
 *
 * MODULE:     v.edit
 *
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym  les-ejk cz>
 *             Martin Landa
 *
 * PURPOSE:    This module edits vector maps. 
 *             Vertex operations.
 *
 * COPYRIGHT:  (C) 2006-2007 by the GRASS Development Team
 *
 *             This program is free software under the
 *             GNU General Public License (>=v2).
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 ****************************************************************/

#include "global.h"

/* 
 * move all vertices in the given bounding box(es)
 *
 * return number of moved vertices
 * return -1 on error
 */
int do_move_vertex(struct Map_info *Map, struct ilist *List, int print,
		   struct Option *coord, double thresh,
		   double move_x, double move_y)
{
    int nvertices_moved;

    int i, j, k;
    int line, newline, type, rewrite;
    double east, north;
    double *x, *y, *z;
    char *moved;

    struct line_pnts *Points;
    struct line_cats *Cats;
    BOUND_BOX bbox;
    
    nvertices_moved = 0;
    moved = NULL;

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();

    for (i = 0; i < List -> n_values; i++) {
	line = List -> value[i];
	
	if (!Vect_line_alive (Map, line))
	    continue;
	
	type = Vect_read_line(Map, Points, Cats, line);

	x = Points -> x;
	y = Points -> y;
	z = Points -> z;

	/* vertex moved ? */
	G_realloc ((void *) moved, Points -> n_points * sizeof (char));
	G_zero (moved, sizeof (char));

	rewrite = 0;
	for (j = 0; coord -> answers[i]; i += 2) {
	    east  = atof (coord -> answers[i]);
	    north = atof (coord -> answers[i+1]);
	    
	    coord2bbox (east, north, thresh, &bbox);

	    G_debug (3, "box: east=%g, north=%g", east, north);

	    /* move all vertices in the bounding box */
	    for (k = 0; k < Points -> n_points; k++) {
		if (!moved[k] && Vect_point_in_box (x[k], y[k], z[k],
						    &bbox)) {
		    G_debug (5, "move vertex index=%d", k);
		    x[k] += move_x;
		    y[k] += move_y;

		    moved[k] = 1;
		    rewrite = 1;

		    nvertices_moved++;
		}
	    } /* for each point at line */
	}
	
	if (rewrite) {
	    newline = Vect_rewrite_line (Map, line, type, Points, Cats);
	    if (newline < 0)  {
		G_warning(_("Cannot rewrite line [%d]"), line);
		return -1;
	    }
	    
	    if (print) {
		fprintf(stdout, "%d%s",
			line,
			i < List->n_values -1 ? "," : "");
		fflush (stdout);
	    }
	    Vect_list_append (List, newline);
	}
    } /* for each selected line */

    /* destroy structures */
    Vect_destroy_line_struct(Points);
    Vect_destroy_cats_struct(Cats);
    G_free ((void *) moved);

    G_message(_("[%d] verteces moved"), nvertices_moved);
    
    return nvertices_moved;
}

/* 
 * breaks (add new vertex) line in the given bounding box(es)
 *
 * return number of added verteces
 * return -1 on error
 */
int do_break(struct Map_info *Map, struct ilist *List, int print,
	     struct Option* coord, double thresh)
{
    int i, j, k;
    int type, line, seg;
    int nlines_broken, broken, line_in_box;
    double east, north;
    double px, py;

    struct line_pnts *Points,*NPoints;
    struct line_cats *Cats;
    BOUND_BOX bbox;

    nlines_broken = 0;
    Points  = Vect_new_line_struct();
    NPoints = Vect_new_line_struct();
    Cats    = Vect_new_cats_struct();

    for (i = 0; i < List -> n_values; i++) {
	line = List -> value[i];

	if (!Vect_line_alive (Map, line))
	    continue;

        type = Vect_read_line(Map, Points, Cats, line);

	G_debug(3, "Breaking line type [%d] number [%d]", type, line);
	
	broken = 0;
	for (j = 0; !broken && coord -> answers[i]; i += 2) {
	    east  = atof (coord -> answers[i]);
	    north = atof (coord -> answers[i+1]);
	    
	    coord2bbox (east, north, thresh, &bbox);

	    /* line in the box ? */
	    line_in_box = 0;
	    for (k = 0; !line_in_box && k < Points -> n_points; k++) {
		if (Vect_point_in_box (Points -> x[k],
				       Points -> y[k],
				       Points -> z[k],
				       &bbox))
		    line_in_box = 1;
	    }
	    
	    if (!line_in_box)
		continue;

	    seg = Vect_line_distance (Points,
				      east, north, 0.0, /* standpoint */
				      WITHOUT_Z,
				      &px, &py, NULL, /* point on line */
				      NULL, NULL, NULL);
	    
	    /* add new vertex */
	    Vect_line_insert_point (Points, seg, px, py, 0.0);
	    broken = 1;
	} /* for each bounding box */

	/* rewrite the line */
	if (broken) {
	    if (Vect_rewrite_line (Map, line, type, NPoints, Cats) < 0) {
		G_warning(_("Cannot rewrite line [%d]"), line);
		return -1;
		
		if (print) {
		    fprintf(stdout, "%d%s",
			    line,
			    i < List->n_values -1 ? "," : "");
		    fflush (stdout);
		}
	    }
	}
    } /* for each line */

    /* destroy structures */
    Vect_destroy_line_struct(Points);
    Vect_destroy_line_struct(NPoints);
    Vect_destroy_cats_struct(Cats);
    
    G_message(_("[%d] lines broken"), line);
    
    return 1;
}

/*
 * remove vertex from line in the given bounading box(es)
 *
 * return number of removed vertices
 * return -1 on error
 */
int do_remove_vertex(struct Map_info *Map, struct ilist *List, int print,
		     struct Option *coord, double thresh)
{
    int i, j, k;
    int type, line, seg;
    int nvertices_removed, rewrite, line_in_box;
    double east, north;
    double dist1, dist2;
    double xo, yo;
    double *x, *y, *z;

    struct line_pnts *Points;
    struct line_cats *Cats;
    BOUND_BOX bbox;

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();
    
    for (i = 0; i < List -> n_values; i++) {
	line = List -> value[i];
	
	if (!Vect_line_alive (Map, line))
	    continue;

        type = Vect_read_line(Map, Points, Cats, line);

	x = Points -> x;
	y = Points -> y;
	z = Points -> z;
	rewrite = 0;
	for (j = 0; coord -> answers[i]; i += 2) {
	    east  = atof (coord -> answers[i]);
	    north = atof (coord -> answers[i+1]);
	    
	    coord2bbox (east, north, thresh, &bbox);

	    /* line in the box ? */
	    line_in_box = 0;
	    for (k = 0; !line_in_box && k < Points -> n_points; k++) {
		if (Vect_point_in_box (x[k], y[k], z[k],
				       &bbox))
		    line_in_box = 1;
	    }
	    
	    if (!line_in_box)
		continue;
    
	    /* find nearest vertex */
	    seg = Vect_line_distance (Points,
				      east, north, 0.0, 
				      WITHOUT_Z,
				      &xo, &yo, NULL,
				      NULL, NULL, NULL);
	    
	    dist1 = Vect_points_distance (xo, yo, 0.0,
					  x[seg-1], y[seg-1], 0.0,
					  WITHOUT_Z);
	    dist2 = Vect_points_distance (xo, yo, 0.0,
					  Points->x[seg], Points->y[seg], 0.0,
					  WITHOUT_Z);
	    if (dist1 < dist2) {
		seg -= 1;
	    }

	    /* remove vertex */
	    Vect_line_delete_point (Points, seg);
	    rewrite = 1;
	} /* for each bounding box */
	
	if (rewrite) {
	    /* rewrite the line */
	    if (Vect_rewrite_line (Map, line, type, Points, Cats) < 0) {
		G_warning (_("Cannot rewrite line [%d]"), line);
		return -1;
	    }
	    
	    if (print) {
		fprintf(stdout, "%d%s",
			line,
			i < List->n_values -1 ? "," : "");
		fflush (stdout);
	    }
	}
    } /* for each line */

    /* destroy structures */
    Vect_destroy_line_struct(Points);
    Vect_destroy_cats_struct(Cats);
    
    G_message(_("[%d] vertices removed"), nvertices_removed);

    return nvertices_removed;
}

/*
 * breaks (split) selected vector line on position(s) given by coord
 *
 * return number of modified lines
 * return -1 on error
 */
int do_split(struct Map_info *Map, struct ilist *List, int print,
	     struct Option* coord, double thresh)
{
    int i, j, k;
    int type, line, seg;
    int nlines_modified, line_in_box;
    double east, north;
    double px, py;
    double *x, *y, *z;

    struct line_pnts *Points, *Points2;
    struct line_cats *Cats;
    BOUND_BOX bbox;

    Points = Vect_new_line_struct();
    Points2 = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();

    for (i = 0; i < List -> n_values; i++) {
	line = List -> value[i];
	
	if (!Vect_line_alive (Map, line))
	    continue;

        type = Vect_read_line(Map, Points, Cats, line);

	x = Points -> x;
	y = Points -> y;
	z = Points -> z;

	for (j = 0; coord -> answers[i]; i += 2) {
	    east  = atof (coord -> answers[i]);
	    north = atof (coord -> answers[i+1]);
	    
	    coord2bbox (east, north, thresh, &bbox);

	    /* line in the box ? */
	    line_in_box = 0;
	    for (k = 0; !line_in_box && k < Points -> n_points; k++) {
		if (Vect_point_in_box (x[k], y[k], z[k],
				       &bbox))
		    line_in_box = 1;
	    }
	    
	    if (!line_in_box)
		continue;

	    seg = Vect_line_distance (Points, east, north, 0.0,
				      WITHOUT_Z,
				      &px, &py, NULL,
				      NULL, NULL, NULL);

	    /* copy first line part */
	    for (j = 0; j < seg; j++) {
		Vect_append_point(Points2,
				  x[j], y[j], z[j]);
	    }
	    
	    /* add last vertex */
	    Vect_append_point(Points2, east, north, 0.0);
	    
	    /* rewrite the line */
	    if (Vect_rewrite_line (Map, line, type, Points2, Cats) < 0)  {
		G_warning(_("Cannot rewrite line [%d]"), line);
		return -1;
	    }
	    
	    Vect_reset_line (Points2);

	    /* add given vertex */
	    Vect_append_point(Points2, east, north, 0.0);

	    /* copy second line part */
	    for (j = seg; j < Points->n_points; j++) {
		Vect_append_point(Points2, 
				  x[j], y[j], z[j]);
	    }

	    /* rewrite the line */
	    if ( Vect_write_line (Map, type, Points2, Cats) < 0)  {
		G_warning(_("Cannot rewrite line [%d]"), line);
		return -1;
	    }

	    if (print) {
		fprintf(stdout, "%d%s",
			line,
			i < List->n_values -1 ? "," : "");
		fflush (stdout);
	    }
	    nlines_modified++;
        } /* for each bounding box */
    } /* for each selected line */

    G_message(_("[%d] lines broken"), line);

    return nlines_modified;
}
