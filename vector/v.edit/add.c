#include "global.h"

int do_add(struct Map_info *Map)
{
    int next_cat;
	    
    next_cat = cat_max_get(1);
    if(strcmp(typ_opt->answer, "point")==0) {
	int i;
	struct line_pnts *Points;

	for(i=0; pnt_opt->answers[i] != NULL; i++) { 
	    double x,y;
	    next_cat++;
	    x = atof(pnt_opt->answers[i]);
	    y = atof(pnt_opt->answers[++i]);
	    G_debug (1, "Adding a point to map [%s], x=%.10f y=%.10f with cat %d",
		     map_opt->answer, x, y, next_cat);
	    
	    Points = Vect_new_line_struct ();
	    Vect_append_point ( Points, x, y, 0 );

	    add_line ( Map, GV_POINT, Points, next_cat );
	    cat_max_set(1, next_cat);
	}
    }
    else {
	G_warning("Sorry this is not yet implemented");
    }
    return 1;
}

int add_line(struct Map_info *Map, int type, struct line_pnts *Points, int cat)
{
    int ret;
    struct line_cats *Cats;
    Cats = Vect_new_cats_struct();
    
    Vect_cat_set ( Cats, 1, cat ); 
    
    ret = Vect_write_line(Map, type, Points, Cats );

//    db_add_values(cat);

    return 1;
}
