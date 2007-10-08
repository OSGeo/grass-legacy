/***************************************************************
 *
 * MODULE:     v.edit
 * 
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym  les-ejk cz>
 *             Martin Landa <landa.martin gmail.com>
 *               
 * PURPOSE:    This module edits vector map.
 *             Copy selected features.
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
   \brief Copy selected features

   \param[in] Map vector map
   \param[in] List list of selected features (to be copied)

   \return number of copied features
   \return -1 on error 
 */
int do_copy (struct Map_info *Map, struct ilist *List)
{
    struct line_cats *Cats;
    struct line_pnts *Points;
    int i;
    int type, line;
    int nlines_copied;

    nlines_copied = 0;
    Cats = Vect_new_cats_struct(); 
    Points = Vect_new_line_struct();

    /* for each line, make a copy */
    for (i = 0; i < List->n_values; i++) {
	line = List -> value[i];

	if (!Vect_line_alive(Map, line))
	    continue;

        type = Vect_read_line(Map, Points, Cats, line);

        G_debug(3, "Copying line type %d number %d", type, line);

        /* copy */
        if (Vect_write_line (Map, type, Points, Cats) < 0) {
            G_warning (_("Unable to write line %d"), line);
	    return -1;
	}
        
	nlines_copied++;
    }

    Vect_destroy_line_struct (Points);
    Vect_destroy_cats_struct (Cats);

    return nlines_copied;
}
