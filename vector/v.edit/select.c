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
 * TODO:       3D
 ****************************************************************/

#include <grass/dbmi.h>
#include "global.h"

struct ilist *select_lines(struct Map_info *Map)
{
    struct ilist *List;

    /* select by category */
    if(cat_opt->answer != NULL) {
        List = sel_by_cat(Map, NULL);
    }
    /* select by id's */
    else if (id_opt->answer != NULL) {
        List = sel_by_id(Map);
    }
    /* select by coordinates */
    else if (coord_opt->answer != NULL) {
        List = sel_by_coordinates(Map);
    }
    /* select by bbox */
    else if (bbox_opt->answer != NULL) {
        List = sel_by_bbox(Map);
    }
    /* select by polygon */
    else if (poly_opt->answer != NULL) {
        List = sel_by_polygon(Map);
    }
    /* select by where statement */
    else if (where_opt->answer != NULL) {
        List = sel_by_where(Map);
    }
    else {
        /* this case should not happen, see args.c for details */
	G_warning(_("At least one option from <%s> must be specified"),
		  "cats, coords, bbox, polygon, id");
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

    if ( List->n_values == 0 ) {
        /* this case should not happen, see args.c for details */
        G_warning(_("At least one option from <%s> must be specified"),
		  "cats, coords, bbox, polygon, id");
    }
    
    G_debug ( 1, "  %d lines selected", List->n_values );

    /* print the result */
    if (List->n_values > 0) {
        for (i = 0; i <= List->n_values-1; i++)
            fprintf(stdout,"%d%s", List->value[i], i < List->n_values-1 ? "," : "\n");
    }
    else {
        G_warning(_("No lines found"));
	return 0;
    }

    return 1;
}

/*
 * select lines by category
 *
 * return line List ilist
 */
struct ilist *sel_by_cat(struct Map_info *Map, struct cat_list *cl_orig)
{
    struct ilist *List, *List_tmp;
    struct cat_list *cl;

    int i, layer, type;

    layer = atoi (fld_opt->answer);
    type  = Vect_option_to_types (type_opt);

    List      = Vect_new_list();
    List_tmp  = Vect_new_list();

    if (cl_orig == NULL) {
	cl = Vect_new_cat_list();

	Vect_str_to_cat_list (cat_opt->answer, cl);
    }
    else {	
	cl = cl_orig;
    }

    for(i = 0; i < cl -> n_ranges; i++) {
        int cat;
        for(cat = cl->min[i]; cat <= cl->max[i]; cat++) {
            Vect_cidx_find_all (Map, layer, type, cat, List_tmp);
	    Vect_list_append_list (List, List_tmp);
        }
    }

    Vect_destroy_list (List_tmp);

    G_debug (1, "  %d lines selected (by category)", List->n_values);

    return List;
}

 /*
 * Select lines by coordinates, 
 * return line List ilist
 */
struct ilist *sel_by_coordinates(struct Map_info *Map)
{
    int id;
    double east,north;

    struct ilist *List;

    double maxdist;
    int i;
    int type = Vect_option_to_types (type_opt);

    maxdist = max_distance (atof(maxdist_opt->answer));

    List = Vect_new_list();

    for (i = 0; coord_opt->answers[i]; i+=2) {
        east = atof(coord_opt->answers[i]);
        north = atof(coord_opt->answers[i+1]);

	/* TODO: 3D */
	id = Vect_find_line (Map, east, north, 0.0, type, maxdist, 0, 0);

        if (id > 0) {
            Vect_list_append (List, id);
        }
    }

    G_debug (1, "  %d lines selected (by coordinates)", List->n_values);

    return List;
}

/*
 * select lines by bbox
 *
 * return line List ilist
 */
struct ilist *sel_by_bbox(struct Map_info *Map)
{
    BOUND_BOX bbox;
    double x1,x2,y1,y2;
    struct ilist *List;
    int type;

    type = Vect_option_to_types (type_opt);

    List = Vect_new_list ();

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

    Vect_select_lines_by_box (Map, &bbox, type, List);
    
    G_debug (1, "  %d lines selected (by bbox)", List->n_values);

    return List;
}

/*
 * select lines by polygon
 *
 * return line List ilist
 */
struct ilist *sel_by_polygon(struct Map_info *Map)
{
    struct ilist *List;
    struct line_pnts *Polygon;
    int i;
    int type;

    type = Vect_option_to_types (type_opt);

    List = Vect_new_list ();
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
    Vect_select_lines_by_polygon (Map, Polygon, 0, NULL, type, List);

    G_debug (1, "  %d lines selected (by polygon)", List->n_values);

    return List;
}

/*
 * select lines by polygon
 *
 * return line List ilist
 */
struct ilist *sel_by_id(struct Map_info *Map)
{
    struct ilist *List;
    int i,j;
    struct cat_list *il; /* NOTE: this is not cat list, but list of id's */

    il = Vect_new_cat_list();
    Vect_str_to_cat_list(id_opt->answer, il);

    List = Vect_new_list ();
    
    for ( i = 1; i <= Vect_get_num_lines (Map); i++) {
        for(j = 0;j < il->n_ranges; j++) {
            int id;
            for(id = il->min[j]; id <= il->max[j]; id++) {
                if (id == i)
                    Vect_list_append (List, id);
            }
        }
    }
    
    G_debug (1, "  %d lines selected (by id)", List->n_values);

    return List;
}

/*
 * select lines according to SQL where statement
 *
 * return line List ilist
 */
struct ilist *sel_by_where (struct Map_info *Map)
{
    struct ilist *List;
    struct cat_list *cat_list;

    struct field_info* Fi;
    dbDriver* driver;
    dbHandle handle;

    int *cats, ncats, field;

    List = Vect_new_list ();

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
    
    List = sel_by_cat (Map, cat_list);

    G_debug (1, "  %d lines selected (by id)", List->n_values);

    return List;
}
