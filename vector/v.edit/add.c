#include "global.h"
static int _get_next_cat(int *ci, struct cat_list * cl, int prev_cat);

/**
 * @todo if same feature exists already, then only add cat */
int do_add(struct Map_info *Map)
{
    int next_cat=-1;
    int layer=atoi(fld_opt->answer);
    int type;
    int i,np, cc=0, ci=0;
    struct line_pnts *line=NULL, *point=NULL;
    struct cat_list * cl=NULL;

    if(cat_opt->answer != NULL) {
	cl = Vect_new_cat_list();
	Vect_str_to_cat_list(cat_opt->answer, cl);
	next_cat=cl->min[ci];
	G_debug(1, "next_cat=%d, cl->n_ranges", next_cat, cl->n_ranges);
    }
    else {
	next_cat = cat_max_get(layer) + 1;
    }

    for(i=0;pnt_opt->answers[i]!= NULL;i++);

    np=i/2;
    if(np==0) {
	fprintf(stderr, "I need at least 1 point!!!\n\n");
	fflush(stderr);
	return 0;
    }
    
    if(strcmp(typ_opt->answer, "point")==0) {
	type = GV_POINT;
    }
    else if(strcmp(typ_opt->answer, "line")==0) {
	type = GV_LINE;
    }
    else if(strcmp(typ_opt->answer, "area")==0) {
	type = GV_AREA;
    }
    else if(strcmp(typ_opt->answer, "boundary")==0) {
	type = GV_BOUNDARY;
    }
    else if(strcmp(typ_opt->answer, "centroid")==0) {
	type = GV_CENTROID;
    }
    else {
	G_warning("Sorry I can not add %s",typ_opt->answer);
	return 0;
    }

    switch(type) {
      case GV_POINT:
      case GV_CENTROID:

	if(np > 2) {
	    G_warning(_("Adding many %s with same attributes"),
		      type==GV_POINT?_("points"):_("centroids"));
	}

	for(i=0; pnt_opt->answers[i] != NULL; i++) {
	    double x,y;
	    int neanode;

	    x = atof(pnt_opt->answers[i]);
	    y = atof(pnt_opt->answers[++i]);

	    if(snap > 0.0) {
		neanode = Vect_find_node(Map, x,y,0,snap,0);
		if(neanode) {
		    Vect_get_node_coor(Map, neanode, &x, &y, NULL);
		}
	    }

	    G_debug (1, "Adding a %s to map [%s], x=%.10f y=%.10f with cat %d",
		     type==GV_POINT?_("point"):_("centroid"),
		     map_opt->answer, x, y, next_cat);

	    point = Vect_new_line_struct ();
	    Vect_append_point ( point, x, y, 0 );

	    if(!add_line ( Map, type, point, layer, next_cat )) {
		return 0;
	    }
	    cat_max_set(layer, next_cat);
	    next_cat = _get_next_cat(&ci, cl, next_cat);
	    /* Vect_destroy_line_struct(point); */
	}
	break;
      case GV_LINE:
	if(np < 2) {
	    fprintf(stderr, _("ERROR: A line needs at least 2 points"));
	    return 0;
	}
      case GV_BOUNDARY:
      case GV_AREA:
	if(type!=GV_LINE) {
	    G_debug(1, "b_flg=%p", b_flg);
	    if(b_flg->answer) {
		cc = next_cat+1;
	    }
	    else {
		cc = next_cat;
		next_cat=0;
	    }
	    if(np < 3 ) {
		fprintf(stderr, _("ERROR: An area needs at least 3 points"));
		return 0;
	    }
	}
	line = Vect_new_line_struct ();
	for(i=0; pnt_opt->answers[i] != NULL; i++) {
	    double x,y;
	    int neanode;

	    x = atof(pnt_opt->answers[i]);
	    y = atof(pnt_opt->answers[++i]);
	    if(snap > 0.0) {
		neanode = Vect_find_node(Map, x,y,0,snap,0);
		if(neanode) {
		    G_debug (1, "x=%.10f y=%.10f nearest node=%d", x, y, 
			     neanode);
		    Vect_get_node_coor(Map, neanode, &x, &y, NULL);
		}
	    }
	    G_debug (1, "Adding a point to line in map [%s], x=%.10f y=%.10f with cat %d",
		     map_opt->answer, x, y, next_cat);

	    Vect_append_point ( line, x, y, 0 );
	}
	if(type != GV_LINE) {
	    if(!c_flg->answer &&
	       (((line->x[0]) != (line->x[line->n_points-1])) ||
	       ((line->y[0]) != (line->y[line->n_points-1]))) )
	    {
		G_warning(_("Boundary not closed. Closing it for you"));
		G_debug (1, "Adding a point to line in map [%s], x=%.10f y=%.10f without cat",
			 map_opt->answer, line->x[0], line->y[0]);
		Vect_append_point ( line, line->x[0], line->y[0], 0 );
	    }

	    if(type==GV_AREA) {
		double xc, yc;
		int ret;

		/* ret = Vect_get_point_in_poly(line, &xc, &yc); */
		ret = Vect_get_point_in_poly_isl(line, NULL, 0, &xc, &yc);
		if(ret < 0) { /* could not find point inside polygon */
		    return 0;
		}
		point = Vect_new_line_struct ();
		Vect_append_point ( point, xc, yc, 0 );
		G_debug (1, "Adding a centroid to area in map [%s], x=%.10f y=%.10f with cat %d",
			 map_opt->answer, xc, yc, cc);
	    }
	}
	if(!add_line ( Map, type==GV_LINE?GV_LINE:GV_BOUNDARY, line, 
		       layer, next_cat ))
	{
	    return 0;
	}
	if(type==GV_AREA) {
	    if(!add_line ( Map, GV_CENTROID, point, layer, cc )) {
		return 0;
	    }
	    cat_max_set(layer, cc);
	    next_cat = _get_next_cat(&ci, cl, cc);
	    /* Vect_destroy_line_struct(point); */
	}
	else {
	    cat_max_set(layer, next_cat);
	    next_cat = _get_next_cat(&ci, cl, next_cat);
	}
	/* Vect_destroy_line_struct(line); */
	break;
      default:
	G_warning("This should not happen");
	return 0;
    }
    return 1;
}

int add_line(struct Map_info *Map, int type, struct line_pnts *Points,
	     int layer, int cat)
{
    struct line_cats *Cats;
    Cats = Vect_new_cats_struct();
    if(Cats== NULL)
	return 0;

    if(cat) {
	if(Vect_cat_set ( Cats, layer, cat ) <=0) {
	    attr_del(Map, layer, cat);
	    fprintf(stderr, "Unable to set cat %d in layer %d\n", cat, layer);
	    return 0;
	}
    }

    if((d_flg->answer) || (cat==0)) {
	if(!Vect_write_line(Map, type, Points, Cats )) {
	    fprintf(stderr, "Unable to write %s to vector", type==GV_POINT?"point":"line\n");
	    return 0;
	}
    }
    else {
	if(!attr_new(Map, layer, cat, val_opt->answer)) {
	    return 0;
	}
	if(!Vect_write_line(Map, type, Points, Cats )) {
	    attr_del(Map, layer, cat);
	    fprintf(stderr, "Unable to write %s to vector\n", type==GV_POINT?"point":"line");
	    return 0;
	}
    }

    return 1;
}

static int _get_next_cat(int *ci, struct cat_list * cl, int prev_cat)
{
    G_debug(1, "cat_opt->answer=%s", cat_opt->answer);
    G_debug(1, "%s:%d prev_cat=%d *ci=%d", __func__, __LINE__, prev_cat, *ci);
    if(cat_opt->answer != NULL) {
    G_debug(1, "%s:%d prev_cat=%d cl->max[*ci]=%d", __func__, __LINE__, prev_cat, cl->max[*ci]);
	if(prev_cat >= cl->max[*ci]){
	    (*ci)++;
	    G_debug(1, "%s:%d *ci=%d, cl->n_ranges", __func__, __LINE__,*ci, cl->n_ranges);
	    if(*ci < cl->n_ranges) {
		G_debug(1, "%s:%d returning %d", __func__, __LINE__,cl->min[*ci]);
		return cl->min[*ci];
	    }
	    else {
		cat_opt->answer = NULL;
		G_debug(1, "%s:%d returning %d", __func__, __LINE__,prev_cat +1);
		return prev_cat+1;
	    }
	}
	else {
	    G_debug(1, "%s:%d returning %d", __func__, __LINE__, prev_cat+1);
	    return prev_cat+1;
	}
    }
    else {
	G_debug(1, "%s:%d returning %d", __func__, __LINE__,prev_cat +1);
	return prev_cat+1;
    }
    
}

/*
int snap(struct Map_info *Map, double *ux, double *uy, double *uz,
	 double maxdist, int with_z )
{
    
}
*/
