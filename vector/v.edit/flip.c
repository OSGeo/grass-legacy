/***************************************************************
 *
 * MODULE:     v.edit
 * 
 * AUTHOR(S):  GRASS Development Team
 *             Martin Landa
 *               
 * PURPOSE:    This module edits vector maps.
 *             Flip direction of selected vector lines.
 *             Inspired by v.flip script by Maciej Sieczka
 *
 * COPYRIGHT:  (C) 2007 The GRASS Development Team
 *
 *             This program is free software under the 
 *             GNU General Public License (>=v2). 
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 **************************************************************/

#include "global.h"

/* 
 * flip direction of selected vector lines (i.e. GV_LINES)
 * return number of modified lines
 * return -1 on error
 */
int do_flip (struct Map_info *Map, struct ilist *List, int print)
{
    struct line_cats *Cats;
    struct line_pnts *Points;
    int i, line, type;
    int nlines_flipped;

    nlines_flipped = 0;

    Points = Vect_new_line_struct();
    Cats   = Vect_new_cats_struct();

    for (i = 0; i < List -> n_values; i++) {
	line = List -> value[i];

	if (!Vect_line_alive (Map, line))
	    continue;
	
	type = Vect_read_line (Map, Points, Cats, line);

	if (!(type & GV_LINES))
	    continue;

	Vect_line_reverse (Points);

	if (Vect_rewrite_line (Map, line, type, Points, Cats) < 0) {
	    G_warning (_("Cannot rewrite line %d"),
		       line);
	    return -1;
	}

	if (print) {
	    fprintf(stdout, "%d%s",
		    List -> value[i],
		    i < List->n_values -1 ? "," : "");
	    fflush (stdout);
	}
	    
	nlines_flipped++;
    }

    /* destroy structures */
    Vect_destroy_line_struct(Points);
    Vect_destroy_cats_struct(Cats);

    G_message(_("%d lines flipped"), nlines_flipped);

    return nlines_flipped;
}
