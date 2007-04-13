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
 */
int do_flip (struct Map_info *Map)
{
    struct ilist *List;
    struct line_cats *Cats;
    struct line_pnts *Points, *Points_flipped;
    int i, line, layer, type;
    int nlines_modified;

    layer = atoi(fld_opt->answer);
    nlines_modified = 0;

    /* select lines */
    List = select_lines (Map);

    if (List -> n_values < 1)
	return 0;

    Points         = Vect_new_line_struct();
    Points_flipped = Vect_new_line_struct();
    Cats           = Vect_new_cats_struct();

    for (i = 0; i < List -> n_values; i++) {
	line = List -> value[i];

	if (!Vect_line_alive (Map, line))
	    continue;
	
	type = Vect_read_line (Map, Points, Cats, line);

	if (!(type & GV_LINES))
	    continue;

	Vect_reset_line (Points_flipped);

	Vect_append_points (Points_flipped, Points, GV_BACKWARD);

	if (Vect_rewrite_line (Map, line, type, Points_flipped, Cats) < 0)
	    G_fatal_error (_("Cannot rewrite line [%d]"),
			   line);

	if (i_flg->answer) {
	    fprintf(stdout,"%d,", line);
	    fflush (stdout);
	}
	    
	nlines_modified++;
    }

    if (i_flg->answer && nlines_modified > 0)
	fprintf(stdout,"\n");

    /* destroy structures */
    Vect_destroy_line_struct(Points);
    Vect_destroy_cats_struct(Cats);
    Vect_destroy_list(List);

    G_message(_("Editing: [%d] lines modified"), nlines_modified);

    return nlines_modified;
}
