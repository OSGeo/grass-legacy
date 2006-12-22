#include "global.h"

int do_del(struct Map_info *Map)
{
    int res = 0;


    /* cats or coord or bbox */
    if(cat_opt->answer != NULL) {
        
        res = delete_categories(Map);
    }
    else if (coord_opt->answer != NULL) {

        res = delete_coordinates(Map);
    }
    else if (bbox_opt->answer != NULL) {

        res = delete_bbox(Map);
    }
    else {
        /* this case should not happen, see args.c for details */
        G_warning("cats, coord or bbox must be specified");
    }

}

int delete_bbox(struct Map_info *Map) 
{
    double x1,y1,x2,y2;
    BOUND_BOX bbox;
    BOUND_BOX feature_bbox;
    int cat, ret, type, i;
    struct ilist *List;
    SPATIAL_INDEX si;
    struct line_pnts *Points;
    struct line_cats *Cats;
    int removed = 0;
    

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

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();
    List = Vect_new_list ();

    /* get lines number */
    Vect_select_lines_by_box ( Map, &bbox, GV_POINTS | GV_LINES | GV_BOUNDARY | GV_CENTROID, List );
    G_debug ( 1, "  %d lines selected by box", List->n_values );

    for ( i = 0; i < List->n_values; i++) {

        type = Vect_read_line(Map, Points, Cats, List->value[i]);
        G_debug(2, "Deleting type [%d] number [%d]", type, List->value[i]);
        Vect_delete_line(Map, List->value[i]); 
        removed++;
        /* attr_del(Map, layer, cat);*/
    }
    G_message(_("%d features deleted"), removed);
    return 1;
}

int delete_coordinates(struct Map_info *Map) 
{
    double east, north;
    int line;
    double maxdist = 0.5;
    char buff[16] = "";

    int type;
    struct line_pnts *Points;
    struct line_cats *Cats;
    int cat;
    int field=atoi(fld_opt->answer);
    int removed = 0;

    east = atof(coord_opt->answers[0]);
    north = atof(coord_opt->answers[1]);

    line = Vect_find_line(Map, east, north, 0.0, GV_POINT | GV_CENTROID, maxdist, 0, 0);

    if (line == 0) 
        line = Vect_find_line(Map, east, north, 0.0, GV_LINE | GV_BOUNDARY | GV_FACE, maxdist, 0, 0);


    G_debug (2, "line = %d", line);

    if (line == 0) {
        G_warning(_("Nothing Found."));
        return 0;
    }
    else {
	Points = Vect_new_line_struct();
	Cats = Vect_new_cats_struct();
        type = Vect_read_line(Map, Points, Cats, line);
        if ((cat = Vect_get_line_cat(Map, line, field )) > 0) 
            sprintf(buff,_("category [%d]"),cat);
            
        G_debug(2, "Deleting type [%d] number [%d] %s", type, line, buff);
        Vect_delete_line(Map, line); 
        removed++;

        /* attr_del(Map, layer, cat);*/
    }

    G_message(_("%d features deleted"), removed);
    return 1;
}


int delete_categories(struct Map_info *Map) 
{
    
    struct cat_list * cl;
    int layer=atoi(fld_opt->answer);
    int i, removed=0;
    int field_idx;

    cl = Vect_new_cat_list();
    Vect_str_to_cat_list(cat_opt->answer, cl);

    field_idx = Vect_cidx_get_field_index( Map, layer);

    G_debug(1,"layer = %d, field_idx = %d",layer, field_idx);

    for(i=0;i<cl->n_ranges;i++) {
	int cat, type, id, ret;
	G_debug(1, "cl->min[%d]=%d, cl->max[%d]=%d, layer=%d",
		i, cl->min[i], i, cl->max[i], cl->field);
	for(cat=cl->min[i]; cat <= cl->max[i]; cat++) {
            /* FIXME
             * this should be done in while(Vect_cidx_find_next){} loop
             */
	    ret = Vect_cidx_find_next(Map, field_idx, cat, GV_POINTS|GV_LINES | GV_BOUNDARY | GV_CENTROID, 
				      0, &type, &id);
	    G_debug(1, "ret=%d", ret);
	    if(ret<0) continue;
            G_debug(2,"Deleting type [%d] number [%d] cat [%d]", type, id, cat);
            Vect_delete_line(Map, id); 
            /* attr_del(Map, layer, cat);*/
            removed ++;
	}
    }
    G_message(_("%d features deleted"), removed);

    return 1;
}
