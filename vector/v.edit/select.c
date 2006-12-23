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

#include "global.h"

int do_select(struct Map_info *Map )
{
    struct ilist *List;
    int i;

    /* select by category */
    if(cat_opt->answer != NULL) {
        List = sel_by_cat(Map);
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
    else {
        /* this case should not happen, see args.c for details */
        G_warning("cats, coord or bbox must be specified");
    }

    /* print the result */
    if (List->n_values > 0) {
        for (i = 0; i <= List->n_values-1; i++) {
            fprintf(stdout,"%d",List->value[i]);
            if (i <= List->n_values-2) 
                fprintf(stdout,",");
        }
        fprintf(stdout,"\n");
        return 1;
    }
    else {
        G_warning(_("No lines found"));
    }
}

/*
 * Select lines by category, 
 * return line List ilist
 */
struct ilist *sel_by_cat(struct Map_info *Map)
{
    struct ilist *List;
    List = Vect_new_list ();
    struct cat_list *cl;
    int field_idx;
    int i;
    int layer=atoi(fld_opt->answer);

    cl = Vect_new_cat_list();
    Vect_str_to_cat_list(cat_opt->answer, cl);
    field_idx = Vect_cidx_get_field_index( Map, layer );
    
    for(i=0;i<cl->n_ranges;i++) {
        int cat;
        for(cat=cl->min[i]; cat <= cl->max[i]; cat++) {
            Vect_cidx_find_all(Map, field_idx,  GV_POINT | GV_CENTROID | GV_LINES | GV_BOUNDARY | GV_FACE, cat,  List);
        }
    }

    return List;
}

 /*
 * Select lines by coordinates, 
 * return line List ilist
 */
struct ilist *sel_by_coordinates(struct Map_info *Map)
{
    double east,north;
    int id;
    struct ilist *List;
    double maxdist = atof(snap_opt->answer);
    int i;

    List = Vect_new_list ();

  
    for (i = 0; coord_opt->answers[i]; i+=2) {
        east = atof(coord_opt->answers[i]);
        north = atof(coord_opt->answers[i+1]);

        if ((id = Vect_find_line(Map, east, north, 0.0, -1, maxdist, 0, 0)) > 0) {
            Vect_list_append ( List, id );
        }
    }

    return List;
}

 /*
 * Select lines by bbox, 
 * return line List ilist
 */
struct ilist *sel_by_bbox(struct Map_info *Map)
{
    BOUND_BOX bbox;
    double x1,x2,y1,y2;
    struct ilist *List;

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

    Vect_select_lines_by_box ( Map, &bbox, GV_CENTROID | GV_POINT | GV_LINES | GV_BOUNDARY | GV_FACE, List );

    return List;
}

 /*
 * Select lines by polygon, 
 * return line List ilist
 */
struct ilist *sel_by_polygon(struct Map_info *Map)
{
    struct ilist *List;
    struct line_pnts *Polygon;
    int i;
    int npoints;

    List = Vect_new_list ();
    Polygon = Vect_new_line_struct();

    
    for (i = 0; poly_opt->answers[i]; i+=2){
        Vect_append_point(Polygon,atof(poly_opt->answers[i]),atof(poly_opt->answers[i+1]), 0.0);
    }
    /* if first and last point of polygon does not match */
    if (atof(poly_opt->answers[i-1]) != atof(poly_opt->answers[0])) {
        Vect_append_point(Polygon,atof(poly_opt->answers[0]),atof(poly_opt->answers[1]), 0.0);
    }

    Vect_select_lines_by_polygon(Map,Polygon, 0, NULL, -1, List);

    return List;
}
