/***************************************************************
 *
 * MODULE:     v.edit
 * 
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym  les-ejk cz>
 *             Martin Landa <landa.martin gmail.com>
 *               
 * PURPOSE:    This module edits vector map.
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

/**
   \brief Delete selected features

   \param[in] Map vector map
   \param[in] List list of features to be deleted

   \return number of deleted features
   \return -1 on on error
 */
int do_delete(struct Map_info *Map, struct ilist *List)
{
    int i, line;
    int nlines_removed;

    nlines_removed = 0;

    /* delete */
    for (i = 0; i < List->n_values; i++) {
	line = List -> value[i];

	if (Vect_line_alive(Map, line)) {
	    if (-1 == Vect_delete_line(Map, line)) { 
		G_warning (_("Unable to delete line %d"), line);
		return -1;
	    }

	    G_debug (3, "Line %d deleted", line);
	    nlines_removed++;
	}
    }

    return nlines_removed;
}
