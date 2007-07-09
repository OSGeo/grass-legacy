/***************************************************************
 *
 * MODULE:     v.edit
 * 
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym  les-ejk cz>
 *             Martin Landa
 *               
 * PURPOSE:    This module edits vector maps.
 *             Delete selected features.
 *
 * COPYRIGHT:  (C) 2007 by the GRASS Development Team
 *
 *             This program is free software under the 
 *             GNU General Public License (>=v2). 
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 **************************************************************/

#include "global.h"

/*
 * delete selected features
 * return number of deleted features
 * return -1 on error
 */
int do_del(struct Map_info *Map, struct ilist *List, int print)
{
    int i, line;
    int nlines_removed;

    nlines_removed = 0;

    /* delete */
    for (i = 0; i < List->n_values; i++) {
	line = List -> value[i];

	if (Vect_line_alive(Map, line)) {
	    G_debug (3, "Line %d deleted", List->value[i] );
	    if (-1 == Vect_delete_line(Map, List->value[i])) { 
		G_warning (_("Cannot delete line %d"), line);
		return -1;
	    }

	    if (print) {
	      fprintf(stdout, "%d%s",
		      line,
		      i < List->n_values -1 ? "," : "");
	      fflush (stdout);
	    }
	    nlines_removed++;
	}
    }

    G_message(_("%d features deleted"), nlines_removed);

    return nlines_removed;
}
