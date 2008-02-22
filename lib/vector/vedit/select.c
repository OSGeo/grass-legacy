/**
   \brief Vedit library - select vector features (by query)

   This program is free software under the
   GNU General Public License (>=v2).
   Read the file COPYING that comes with GRASS
   for details.

   \author (C) 2007-2008 by the GRASS Development Team
   Martin Landa <landa.martin gmail.com>

   \date 2007-2008
*/

#include "vedit.h"

static int select_by_query(struct Map_info *, int, int, int,
			   int, double,
			   struct line_pnts *, struct line_cats *);

/**
   \brief Select features by query (based on geometry)

   Currently supported:
    - QUERY_LENGTH, select all lines longer then threshold (or shorter if threshold is negative)
    - QUERY_DANGLE, select all dangles then threshold (or shorter if threshold is negative)

   If <em>List</em> is not empty query only those feature, otherwise query all
   vector features stored in the vector map.

   \todo Rewrite dangle part to use Vector library functionality

   \param[in] Map vector map
   \param[in] type feature type
   \param[in] layer layer number
   \param[in] thresh threshold value (< 0 for 'shorter', > 0 for 'longer')
   \param[in] query query (length, dangle, ...)
   \param[in,out] List list of selected features
 
   \return number of selected lines
*/
int Vedit_select_by_query(struct Map_info *Map,
			  int type, int layer, double thresh, int query,
			  struct ilist* List)
{
    int num, line, i;
    struct line_pnts *Points;
    struct line_cats *Cats;
    struct ilist *List_query;
    
    Points = Vect_new_line_struct();
    Cats   = Vect_new_cats_struct();

    List_query = Vect_new_list();

    if (List->n_values > 0) { /* query only selected */
	for (i = 0; i < List->n_values; i++) {
	    line = List->value[i];
	    if (select_by_query(Map, line, type, layer,
				query, thresh,
				Points, Cats) == 1) {
		if (!Vect_val_in_list(List, line)) {
		    Vect_list_append(List, line);
		}
	    }
	    else {
		if (Vect_val_in_list(List, line)) {
		    Vect_list_delete(List, line);
		    i--;
		}
	    }
	}
    }
    else { /* global query */
	num = Vect_get_num_lines (Map);
	for (line = 1; line <= num; line++) {
	    if (select_by_query(Map, line, type, layer,
				query, thresh,
				Points, Cats) == 1) {
		Vect_list_append(List, line);
	    }
	}
    }

    G_debug (3, "Vedit_select_by_query(): %d lines selected (by query %d)", List -> n_values, query);

    Vect_destroy_line_struct(Points);
    Vect_destroy_cats_struct(Cats);
    Vect_destroy_list(List_query);

    return List -> n_values; 
}

/**
   \brief Query given line

   \return 1 line test positive 
   \return 0 line test negative
   \return -1 on error (line is dead)
*/
static int select_by_query(struct Map_info *Map, int line, int type, int layer,
			   int query, double thresh,
			   struct line_pnts* Points, struct line_cats* Cats) 
{
    int ltype, cat;
    double length;

    if (!Vect_line_alive(Map, line))
	return -1;
    
    ltype = Vect_read_line(Map, Points, Cats, line);
    Vect_cat_get(Cats, layer, &cat); /* get first category from layer */
    
    if (!(ltype & type))
	return -1;
    
    if (query == QUERY_LENGTH) {
	length = Vect_line_length(Points);
	if (thresh <= 0.0) { /* shorter then */
	    if (length <= fabs(thresh))
		return 1;
	}
	else { /* longer then */
	    if (length > thresh)
		return 1;
	}
    }
    else if (query == QUERY_DANGLE) {
	if (!(type & GV_LINES))
	    return -1;
	/* check if line is dangle */
	
	int i, cat_curr;
	int node1, node2, node;        /* nodes */
	int nnode1, nnode2;            /* number of line in node */
	double nx, ny, nz;             /* node coordinates */
	struct ilist *exclude, *found; /* line id of nearest lines */
	struct line_cats *Cats_curr;   
	
	Vect_get_line_nodes(Map, line, &node1, &node2);
	
	node = -1;
	nnode1 = Vect_get_node_n_lines(Map, node1);
	nnode2 = Vect_get_node_n_lines(Map, node2);
	
	if ((nnode1 == 4 && nnode2 == 1) ||
	    (nnode1 == 1 && nnode2 == 4)) {
	    if (nnode1 == 4)
		node = node1;
	    else
		node = node2;
	}
	
	/* no dangle ? */
	if (node == -1)
	    return -1;
	
	length = Vect_line_length(Points);
	if (thresh <= 0.0) { /* shorter then */
		if (length > fabs(thresh))
		    return -1;
	}
	else { /* longer then */
	    if (length <= thresh)
		return -1;
	}
	
	/* at least one of the lines need to have same category number */
	exclude = Vect_new_list();
	found   = Vect_new_list();
	
	Vect_get_node_coor(Map, node, &nx, &ny, &nz);
	
	Vect_list_append(exclude, line);
	Vect_find_line_list(Map, nx, ny, nz,
			    GV_LINES, 0.0, WITHOUT_Z,
				exclude, found);
	
	Cats_curr = Vect_new_cats_struct();
	
	for (i = 0; i < found->n_values; i++) {
	    Vect_read_line(Map, NULL, Cats_curr, found->value[i]);
	    if (Vect_cat_get(Cats_curr, layer, &cat_curr) > -1) {
		if (cat == cat_curr)
		    return 1;
	    }
	    }
	
	Vect_destroy_cats_struct(Cats_curr);
	Vect_destroy_list(exclude);
	Vect_destroy_list(found);
    }
    else {
	/* this shouldn't happen */
	G_fatal_error ("Vedit_select_by_query(): %s", _("Unknown query tool"));
    }

    return 0;
}
