#include "global.h"

int do_del(struct Map_info *Map)
{
    int i;
    struct cat_list * cl;
    int layer=atoi(fld_opt->answer);

    G_debug(1,"layer = %d",layer);
/*
    if(cat_opt->answer=NULL) {
	cats = coords_catlist(Map);
    }
 */
    cl = Vect_new_cat_list();
    Vect_str_to_cat_list(cat_opt->answer, cl);
    
    for(i=0;i<cl->n_ranges;i++) {
	int cat, type, id, ret;
	G_debug(1, "cl->min[%d]=%d, cl->max[%d]=%d, layer=%d",
		i, cl->min[i], i, cl->max[i], cl->field);
	for(cat=cl->min[i]; cat <= cl->max[i]; cat++) {
	    ret = Vect_cidx_find_next(Map, layer, cat, GV_POINTS|GV_LINES, 
				      0, &type, &id);
	    G_debug(1, "ret=%d", ret);
	    if(ret<0) continue;
	    G_debug(1, "Found something to delete: id=%d, type=%d",id,type);
	    if(type==GV_CENTROID) {
		int area;
		double x,y;
		Vect_get_node_coor(Map, id, &x, &y, NULL);
		area = Vect_find_area(Map, x, y);
		G_debug(1, "Deleteing area %d: id=%d, area=%d",cat,id,area);
/* 		Vect_delete_line(Map, id); */
/* 		Vect_delete_line(Map, area); */
		attr_del(Map, layer, cat);
	    }
	    else {
/* 		Vect_delete_line(Map, id); */
		attr_del(Map, layer, cat);
	    }
	}
    }

    return 1;
}
