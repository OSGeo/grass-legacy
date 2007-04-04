/****************************************************************
 *
 * MODULE:     v.edit
 *
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym  les-ejk cz>
 *
 * PURPOSE:    This module edits vector maps. It is inteded to be mainly
 * 	       used by the the new v.digit GUI.
 *
 * COPYRIGHT:  (C) 2002-2006 by the GRASS Development Team
 *
 *             This program is free software under the
 *             GNU General Public License (>=v2).
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 * TODO:       
 ****************************************************************/

#include <grass/dbmi.h>
#include "global.h"

struct ilist *select_lines(struct Map_info *Map)
{
    struct ilist *List;

    List = Vect_new_list();

    /* select by category */
    if(cat_opt->answer != NULL) {
        sel_by_cat(Map, NULL, List);
    }

    /* select by id's */
    if (id_opt->answer != NULL) {
	sel_by_id(Map, List);
    }
    
    /* select by coordinates (+threshold) */
    if (coord_opt->answer != NULL) {
        sel_by_coordinates(Map, List);
    }
    
    /* select by bbox (TODO: threshold) */
    if (bbox_opt->answer != NULL) {
        sel_by_bbox(Map, List);
    }
    
    /* select by polygon (TODO: threshold) */
    if (poly_opt->answer != NULL) {
        sel_by_polygon(Map, List);
    }
    
    /* select by where statement */
    if (where_opt->answer != NULL) {
        sel_by_where(Map, List);
    }
    
    G_message (_("[%d] of [%d] features selected"),
	       List -> n_values,
	       Vect_get_num_lines (Map));

    return List;
}

/* 
 * print selected vector features
 *
 * return 0 if no vector feature found
 * return 1 if at least one vector feature found
 */
int do_print_selected(struct Map_info *Map)
{
    struct ilist *List;
    int i;

    List = select_lines(Map);

    G_debug ( 1, "  %d lines selected", List->n_values );

    /* print the result */
    for (i = 0; i < List->n_values; i++) {
	fprintf(stdout,"%d%s", List->value[i],
		(i < List->n_values-1) ? "," : "\n");
    }

    return 1;
}

/*
 * select lines by category
 *
 * return number of selected lines
 */
int sel_by_cat(struct Map_info *Map, struct cat_list *cl_orig,
	       struct ilist* List)
{
    struct ilist *List_tmp, *List_tmp1;
    struct cat_list *cl;

    int i, layer, type, cat, nlines;

    nlines = List -> n_values;

    if (nlines == 0) {
	List_tmp = List;
    }
    else {
	List_tmp = Vect_new_list();
    }

    List_tmp1 = Vect_new_list();

    layer = atoi (fld_opt->answer);
    type  = Vect_option_to_types (type_opt);

    if (cl_orig == NULL) {
	cl = Vect_new_cat_list();

	Vect_str_to_cat_list (cat_opt->answer, cl);
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

    /* merge lists (only duplicate items) */
    if (List_tmp != List) {
	merge_lists (List, List_tmp);
	Vect_destroy_list (List_tmp);
    }

    Vect_destroy_list (List_tmp1);

    G_debug (1, "  %d lines selected (by category)", List->n_values);

    return List -> n_values;
}

/*
 * select lines by coordinates
 *
 * return number of selected lines
 */
int sel_by_coordinates(struct Map_info *Map,
		       struct ilist* List)
{
    int id, i, type, nlines;
    double east, north, maxdist;

    struct ilist* List_tmp;

    nlines = List -> n_values;

    if (nlines == 0) {
	List_tmp = List;
    }
    else {
	List_tmp = Vect_new_list();
    }
    
    type    = Vect_option_to_types (type_opt);
    maxdist = max_distance (atof(maxdist_opt->answer));

    for (i = 0; coord_opt->answers[i]; i+=2) {
        east  = atof(coord_opt->answers[i]);
        north = atof(coord_opt->answers[i+1]);

	/* TODO: 3D */
	/* TODO: find all lines in given threshold */
	id = Vect_find_line (Map, east, north, 0.0, type, maxdist, 0, 0);

        if (id > 0) {
            Vect_list_append (List, id);
        }
    }

    /* merge lists (only duplicate items) */
    if (List_tmp != List) {
	merge_lists (List, List_tmp);
	Vect_destroy_list (List_tmp);
    }

    G_debug (1, "  %d lines selected (by coordinates)", List->n_values);

    return List -> n_values;
}

/*
 * select lines by bbox
 *
 * return number of selected lines
 */
int sel_by_bbox(struct Map_info *Map,
		struct ilist* List)
{
    BOUND_BOX bbox;
    double x1, x2, y1, y2;
    int type, nlines;

    struct ilist* List_tmp;

    nlines = List -> n_values;

    if (nlines == 0) {
	List_tmp = List;
    }
    else {
	List_tmp = Vect_new_list();
    }
    
    type = Vect_option_to_types (type_opt);

    /* bounding box */
    x1 = atof(bbox_opt->answers[0]);
    y1 = atof(bbox_opt->answers[1]);
    x2 = atof(bbox_opt->answers[2]);
    y2 = atof(bbox_opt->answers[3]);
    bbox.N = y1 < y2 ? y2 : y1;
    bbox.S = y1 < y2 ? y1 : y2;
    bbox.W = x1 < x2 ? x1 : x2;
    bbox.E = x1 < x2 ? x2 : x1;
    bbox.T = 0.0;
    bbox.B = 0.0;

    Vect_select_lines_by_box (Map, &bbox, type, List_tmp);

    /* merge lists (only duplicate items) */
    if (List_tmp != List) {
	merge_lists (List, List_tmp);
	Vect_destroy_list (List_tmp);
    }
    
    G_debug (1, "  %d lines selected (by bbox)", List->n_values);

    return List -> n_values;
}

/*
 * select lines by polygon
 *
 * return number of selected lines
 */
int sel_by_polygon(struct Map_info *Map,
		   struct ilist* List)
{
    struct ilist *List_tmp;
    struct line_pnts *Polygon;

    int i, type, nlines;

    nlines = List -> n_values;

    if (nlines == 0) {
	List_tmp = List;
    }
    else {
	List_tmp = Vect_new_list();
    }

    type = Vect_option_to_types (type_opt);

    Polygon = Vect_new_line_struct();
    
    for (i = 0; poly_opt->answers[i]; i+=2){
        Vect_append_point(Polygon,
			  atof(poly_opt->answers[i]),
			  atof(poly_opt->answers[i+1]),
			  0.0);
    }
    
    /* if first and last point of polygon does not match */
    if (atof(poly_opt->answers[i-1]) != atof(poly_opt->answers[0])) {
        Vect_append_point(Polygon,
			  atof(poly_opt->answers[0]),
			  atof(poly_opt->answers[1]),
			  0.0);
    }

    /* no isles */
    Vect_select_lines_by_polygon (Map, Polygon, 0, NULL, type, List_tmp);

    /* merge lists (only duplicate items) */
    if (List_tmp != List) {
	merge_lists (List, List_tmp);
	Vect_destroy_list (List_tmp);
    }

    G_debug (1, "  %d lines selected (by polygon)", List->n_values);

    return List -> n_values;
}

/*
 * select lines by id
 *
 * return number of selected lines
 */
int sel_by_id(struct Map_info *Map,
	      struct ilist* List)
{
    int i, j;
    int nlines, num, id;
    struct cat_list *il; /* NOTE: this is not cat list, but list of id's */
    struct ilist *List_tmp;

    nlines = List -> n_values;

    if (nlines == 0) {
	List_tmp = List;
    }
    else {
	List_tmp = Vect_new_list();
    }

    il = Vect_new_cat_list();
    Vect_str_to_cat_list (id_opt->answer, il);

    num = Vect_get_num_lines (Map);

    for (i = 1; i <= num; i++) {
        for(j = 0; j < il->n_ranges; j++) {
	    for(id = il->min[j]; id <= il->max[j]; id++) {
                if (id == i) {
		    Vect_list_append (List_tmp, id);
		}
            }
        }
    }
    
    G_debug (1, "  %d lines selected (by id)", List->n_values);

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
		  struct ilist* List)
{
    struct cat_list *cat_list;
    struct ilist *List_tmp;
    struct field_info* Fi;
    dbDriver* driver;
    dbHandle handle;

    int *cats, ncats, field, nlines;

    nlines = List -> n_values;

    if (nlines == 0) {
	List_tmp = List;
    }
    else {
	List_tmp = Vect_new_list();
    }

    cat_list = Vect_new_cat_list();

    field = atoi (fld_opt -> answer);

    if (field < 1) {
	G_fatal_error (_("Layer must be > 0 for 'where'."));
    }

    Fi = Vect_get_field (Map, field);

    if (!Fi) {
	G_fatal_error (_("No layer database connection."));
    }

    driver = db_start_driver (Fi -> driver);

    if (!driver)
	G_fatal_error(_("Cannot open driver <%s>"),
		      Fi->driver) ;
    
    db_init_handle (&handle);

    db_set_handle (&handle, Fi -> database, NULL);

    if (db_open_database(driver, &handle) != DB_OK)
	G_fatal_error(_("Cannot open database <%s>"),
		      Fi->database);
    
    ncats = db_select_int (driver, Fi -> table, Fi -> key,
			   where_opt->answer, &cats);

    db_close_database(driver);
    db_shutdown_driver(driver);

    Vect_array_to_cat_list (cats, ncats, cat_list);
    
    /* free array of cats */
    if (ncats >= 0)
	G_free (cats);
    
    sel_by_cat (Map, cat_list, List_tmp);

    /* merge lists (only duplicate items) */
    if (List_tmp != List) {
	merge_lists (List, List_tmp);
	Vect_destroy_list (List_tmp);
    }

    Vect_destroy_cat_list (cat_list);

    G_debug (1, "  %d lines selected (by id)", List->n_values);

    return List -> n_values;
}

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
