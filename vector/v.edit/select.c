/****************************************************************
 *
 * MODULE:     v.edit
 *
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym  les-ejk cz>
 *             Martin Landa
 *
 * PURPOSE:    This module edits vector maps. 
 *             Select vector features.
 *
 * COPYRIGHT:  (C) 2006-2007 by the GRASS Development Team
 *
 *             This program is free software under the
 *             GNU General Public License (>=v2).
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 ****************************************************************/

#include <grass/dbmi.h>
#include "global.h"

static char first_selection = 1;

struct ilist *select_lines(struct Map_info *Map, enum mode action_mode,
			   struct GParams *params,
			   struct ilist *List)
{
    int layer, type;
    double thresh;

    layer  = atoi (params -> fld -> answer);
    type   = Vect_option_to_types (params -> type);
    thresh = atof (params -> maxdist -> answer);

    /* select by id's */
    if (params -> id -> answer != NULL) {
	sel_by_id(Map,
		  layer, type, params -> id -> answer,
		  List);
    }

    /* select by category (ignore tools catdel and catadd) */
    if((action_mode != MODE_CATADD && action_mode != MODE_CATDEL) &&
       params -> cat -> answer != NULL) {
	sel_by_cat(Map, NULL,
		   layer, type, params -> cat -> answer,
		   List);
    }
    
    /* select by coordinates (+threshold) */
    if (params -> coord -> answer != NULL) {
        sel_by_coordinates(Map,
			   layer, type, params -> coord, thresh,
			   List);
    }
    
    /* select by bbox (TODO: threshold) */
    if (params -> bbox -> answer != NULL) {
        sel_by_bbox(Map,
		    layer, type, params -> bbox,
		    List);
    }
    
    /* select by polygon (TODO: threshold) */
    if (params -> poly -> answer != NULL) {
        sel_by_polygon(Map,
		       layer, type, params -> poly,
		       List);
    }
    
    /* select by where statement */
    if (params -> where -> answer != NULL) {
        sel_by_where(Map,
		     layer, type, params -> where -> answer,
		     List);
    }

    if (params -> reverse -> answer) {
	reverse_selection(Map, &List);
    }

    G_message (_("%d of %d features selected"),
	       List -> n_values,
	       Vect_get_num_lines (Map));

    return List;
}

/* 
 * print selected vector features
 *
 * return number of selected features
 * return -1 on error
 */
int do_print_selected(struct ilist *List)
{
    int i;

    /* print the result */
    for (i = 0; i < List->n_values; i++) {
	fprintf(stdout, "%d%s",
		List -> value[i],
		i < List->n_values -1 ? "," : "");
    }
    
    return List -> n_values;
}

/*
 * select lines by category
 *
 * return number of selected lines
 */
int sel_by_cat(struct Map_info *Map, struct cat_list *cl_orig,
	       int layer, int type, char *cats,
	       struct ilist* List)
{
    struct ilist *List_tmp, *List_tmp1;
    struct cat_list *cl;

    int i, cat;

    if (first_selection || cl_orig) {
	List_tmp = List;
	first_selection = 0;
    }
    else {
	List_tmp = Vect_new_list();
    }

    List_tmp1 = Vect_new_list();

    if (cl_orig == NULL) {
	cl = Vect_new_cat_list();

	Vect_str_to_cat_list (cats, cl);
    }
    else {	
	cl = cl_orig;
    }

    for(i = 0; i < cl -> n_ranges; i++) {
        for(cat = cl->min[i]; cat <= cl->max[i]; cat++) {
            Vect_cidx_find_all (Map, layer, type, cat, List_tmp1);
	    Vect_list_append_list (List_tmp, List_tmp1);
        }
    }

    G_debug (1, "  %d lines selected (by category)", List_tmp -> n_values);

    /* merge lists (only duplicate items) */
    if (List_tmp != List) {
	merge_lists (List, List_tmp);
	Vect_destroy_list (List_tmp);
    }

    Vect_destroy_list (List_tmp1);

    return List -> n_values;
}

/*
 * select lines by coordinates
 *
 * return number of selected lines
 */
int sel_by_coordinates(struct Map_info *Map,
		       int layer, int type, struct Option *coords, double thresh,
		       struct ilist* List)
{
    int i;
    double east, north, maxdist;

    struct ilist* List_tmp, *List_in_box;
    struct line_pnts *box;

    if (first_selection) {
	List_tmp = List;
	first_selection = 0;
    }
    else {
	List_tmp = Vect_new_list();
    }
    
    box = Vect_new_line_struct();
    List_in_box = Vect_new_list();

    maxdist = max_distance (thresh);

    for (i = 0; coords -> answers[i]; i+=2) {
        east  = atof(coords -> answers[i]);
        north = atof(coords -> answers[i+1]);

	coord2bbox (east, north, maxdist, box);

	Vect_select_lines_by_polygon(Map, box, 0, NULL, type, List_in_box);
	
	if (List_in_box -> n_values > 0) 
	    Vect_list_append_list (List_tmp, List_in_box);
    }

    G_debug (1, "  %d lines selected (by coordinates)", List_tmp -> n_values);

    /* merge lists (only duplicate items) */
    if (List_tmp != List) {
	merge_lists (List, List_tmp);
	Vect_destroy_list (List_tmp);
    }
    
    Vect_destroy_line_struct (box);
    Vect_destroy_list (List_in_box);

    return List -> n_values;
}

/*
 * select lines by bbox
 *
 * return number of selected lines
 */
int sel_by_bbox(struct Map_info *Map,
		int layer, int type, struct Option *bbox_opt,
		struct ilist* List)
{
    BOUND_BOX bbox;
    double x1, x2, y1, y2;

    struct ilist* List_tmp;

    if (first_selection) {
	List_tmp = List;
	first_selection = 0;
    }
    else {
	List_tmp = Vect_new_list();
    }
    
    /* bounding box */
    x1 = atof(bbox_opt -> answers[0]);
    y1 = atof(bbox_opt -> answers[1]);
    x2 = atof(bbox_opt -> answers[2]);
    y2 = atof(bbox_opt -> answers[3]);
    bbox.N = y1 < y2 ? y2 : y1;
    bbox.S = y1 < y2 ? y1 : y2;
    bbox.W = x1 < x2 ? x1 : x2;
    bbox.E = x1 < x2 ? x2 : x1;
    bbox.T = 0.0;
    bbox.B = 0.0;

    Vect_select_lines_by_box (Map, &bbox, type, List_tmp);

    G_debug (1, "  %d lines selected (by bbox)", List_tmp -> n_values);

    /* merge lists (only duplicate items) */
    if (List_tmp != List) {
	merge_lists (List, List_tmp);
	Vect_destroy_list (List_tmp);
    }
    
    return List -> n_values;
}

/*
 * select lines by polygon
 *
 * return number of selected lines
 */
int sel_by_polygon(struct Map_info *Map,
		   int layer, int type, struct Option *poly,
		   struct ilist* List)
{
    struct ilist *List_tmp;
    struct line_pnts *Polygon;

    int i;

    if (first_selection) {
	List_tmp = List;
	first_selection = 0;
    }
    else {
	List_tmp = Vect_new_list();
    }

    Polygon = Vect_new_line_struct();
    
    for (i = 0; poly -> answers[i]; i+=2){
        Vect_append_point(Polygon,
			  atof(poly -> answers[i]),
			  atof(poly -> answers[i+1]),
			  0.0);
    }
    
    /* if first and last point of polygon does not match */
    if (atof(poly -> answers[i-1]) != atof(poly -> answers[0])) {
        Vect_append_point(Polygon,
			  atof(poly -> answers[0]),
			  atof(poly -> answers[1]),
			  0.0);
    }

    /* no isles */
    Vect_select_lines_by_polygon (Map, Polygon, 0, NULL, type, List_tmp);

    G_debug (1, "  %d lines selected (by polygon)", List_tmp -> n_values);

    /* merge lists (only duplicate items) */
    if (List_tmp != List) {
	merge_lists (List, List_tmp);
	Vect_destroy_list (List_tmp);
    }

    return List -> n_values;
}

/*
 * select lines by id
 *
 * return number of selected lines
 */
int sel_by_id(struct Map_info *Map,
	      int layer, int type, char *ids,
	      struct ilist* List)
{
    int i, j;
    int num, id;
    struct cat_list *il; /* NOTE: this is not cat list, but list of id's */
    struct ilist *List_tmp;

    if (first_selection) {
	List_tmp = List;
	first_selection = 0;
    }
    else {
	List_tmp = Vect_new_list();
    }

    il = Vect_new_cat_list();
    Vect_str_to_cat_list (ids, il);

    num = Vect_get_num_lines (Map);

    for(i = 0; i < il -> n_ranges; i++) {
	for(id = il -> min[i]; id <= il -> max[i]; id++) {
	    for (j = 1; j <= num; j++) {
		if (id == j) {
		    Vect_list_append (List_tmp, id);
		}
            }
        }
    }
    
    G_debug (1, "  %d lines selected (by id)", List_tmp -> n_values);

    /* merge lists (only duplicate items) */
    if (List_tmp != List) {
	merge_lists (List, List_tmp);
	Vect_destroy_list (List_tmp);
    }

    Vect_destroy_cat_list (il);

    return List -> n_values; 
}

/*
 * select lines according to SQL where statement
 *
 * return number of selected lines
 */
int sel_by_where (struct Map_info *Map,
		  int layer, int type, char *where,
		  struct ilist* List)
{
    struct cat_list *cat_list;
    struct ilist *List_tmp;
    struct field_info* Fi;
    dbDriver* driver;
    dbHandle handle;

    int *cats, ncats;

    if (first_selection) {
	List_tmp = List;
	first_selection = 0;
    }
    else {
	List_tmp = Vect_new_list();
    }

    cat_list = Vect_new_cat_list();

    if (layer < 1) {
	G_fatal_error (_("Layer must be > 0 for 'where'."));
    }

    Fi = Vect_get_field (Map, layer);

    if (!Fi) {
	G_fatal_error (_("No layer database connection."));
    }

    driver = db_start_driver (Fi -> driver);

    if (!driver)
	G_fatal_error(_("Unable to open driver <%s>"),
		      Fi->driver) ;
    
    db_init_handle (&handle);

    db_set_handle (&handle, Fi -> database, NULL);

    if (db_open_database(driver, &handle) != DB_OK)
	G_fatal_error(_("Unable to open database <%s>"),
		      Fi->database);
    
    ncats = db_select_int (driver, Fi -> table, Fi -> key,
			   where, &cats);

    db_close_database(driver);
    db_shutdown_driver(driver);

    Vect_array_to_cat_list (cats, ncats, cat_list);
    
    /* free array of cats */
    if (ncats >= 0)
	G_free (cats);
    
    sel_by_cat (Map, cat_list,
		layer, type, NULL,
		List_tmp);

    G_debug (1, "  %d lines selected (by where)", List_tmp -> n_values);

    /* merge lists (only duplicate items) */
    if (List_tmp != List) {
	merge_lists (List, List_tmp);
	Vect_destroy_list (List_tmp);
    }

    Vect_destroy_cat_list (cat_list);

    return List -> n_values;
}

/* merge two list, i.e. store only duplicate items */
int merge_lists (struct ilist* alist, struct ilist* blist)
{
    int i;

    struct ilist* list_del;
    
    list_del = Vect_new_list();

    for (i = 0; i < alist -> n_values; i++) {
	if (!Vect_val_in_list (blist, alist -> value[i]))
	    Vect_list_append (list_del, alist -> value[i]);
    }

    Vect_list_delete_list (alist, list_del);

    Vect_destroy_list (list_del);

    return alist -> n_values;
} 

/* reverse selection */
int reverse_selection (struct Map_info *Map, struct ilist** List) {

    struct ilist* list_reverse;
    int line, nlines;

    list_reverse = Vect_new_list();

    nlines = Vect_get_num_lines(Map);

    for (line = 1; line <= nlines; line++) {
	if (!Vect_val_in_list (*List, line))
	    Vect_list_append (list_reverse, line);
    }

    Vect_destroy_list (*List);
    *List = list_reverse;

    return 1;
}
