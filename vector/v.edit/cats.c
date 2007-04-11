/***************************************************************
 *
 * MODULE:     v.edit
 * 
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym  les-ejk cz>
 *             Radim Blazek, Martin Landa
 *               
 * PURPOSE:    This module edits vector maps.
 *
 * COPYRIGHT:  (C) 2001-2007 The GRASS Development Team
 *
 *             This program is free software under the 
 *             GNU General Public License (>=v2). 
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 **************************************************************/

#include "global.h"

/* cats 
 * edit category numbers of selected vector features
 * return number of modified features
 */
int cats (struct Map_info *Map, int del)
{
    int i, j;
    struct ilist *List;
    struct line_cats *Cats;
    struct line_pnts *Points;
    struct cat_list *Clist;
    int line, type, cat, layer;
    int nlines_modified, rewrite;
    
    layer = atoi(fld_opt->answer);
    nlines_modified = 0;

    /* get list of categories */
    Clist = Vect_new_cat_list();
    if (Vect_str_to_cat_list(cat_opt->answer, Clist)) 
	G_fatal_error(_("Could not get cat list <%s>"), cat_opt->answer);
    
    /* features defined by cats */
    if(cat_opt->answer != NULL && Clist->n_ranges > 0) {
	/* select lines */
	List = select_lines (Map);
	
	if (List -> n_values < 1)
	    return 0;

	Cats   = Vect_new_cats_struct (); 
	Points = Vect_new_line_struct();

	/* for each line, set new category */
	for (i = 0; i < List->n_values; i++) {
	    line = List->value[i];
            type = Vect_read_line(Map, Points, Cats, line);

	    if (!Vect_line_alive (Map, line))
		continue;

	    rewrite = 0;
	    for (j = 0; j < Clist -> n_ranges; j++) {
		for (cat = Clist -> min[j]; cat <= Clist -> max[j]; cat++) {
		    /* add new category */
		    if (!del) {
			if(Vect_cat_set (Cats, layer, cat) < 1) {
			    G_warning (_("Cannot set category [%d] line [%d]"),
				       cat, line);
			}
			else {
			    rewrite = 1;
			}
		    }
		    else { /* delete old category */
			if(Vect_field_cat_del (Cats, layer, cat) == 0) {
			    G_warning (_("Cannot delete layer/category [%d/%d] line [%d]"), 
				       layer, cat, line);
			}
			else {
			    rewrite = 1;
			}
		    }
		}
	    }

	    if (rewrite == 0)
		continue;

	    if (Vect_rewrite_line (Map, line, type, Points, Cats) < 0)  {
		G_fatal_error (_("Cannot rewrite line [%d]"), line);
	    }

	    nlines_modified++;

	    if (i_flg->answer) {
		fprintf (stdout,"%d", line);
		fflush (stdout);
	    }
	}
	/* destroy structures */
	Vect_destroy_line_struct(Points);
	Vect_destroy_cats_struct(Cats);
	Vect_destroy_list(List);
    }

    G_message(_("Editing: [%d] lines modified"), nlines_modified);

    return nlines_modified;
}
