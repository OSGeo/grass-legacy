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

int do_move_vertex(struct Map_info *Map)
{
    int res;

    double east, north;
    int line, j;
    double maxdist;
    char buff[16] = "";
    float move[2];

    int cat;
    int type;
    int field;

    struct line_pnts *Points;
    struct line_cats *Cats;

    /* for easier selection of points */
    BOUND_BOX bbox;
    struct ilist *List;

    line = res = 0;
    maxdist = 0.;
    field = atoi(fld_opt->answer);

    if (coord_opt->answers){
        east  = atof(coord_opt->answers[0]);
        north = atof(coord_opt->answers[1]);
    }

    move[0] = atof(move_opt->answers[0]);
    move[1] = atof(move_opt->answers[1]);

    maxdist =  max_distance (atof(maxdist_opt->answer));

    List = Vect_new_list ();

    /* try to find bounding box */
    if (bbox_opt->answers) {
        float x1,x2,y1,y2;
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
    }
    /* try to defined boundinbg box from snapping distance */
    else if (maxdist) {
        bbox.N = north+maxdist;
        bbox.S = north-maxdist;
        bbox.W = east-maxdist;
        bbox.E = east+maxdist;
        bbox.T = 0.0;
        bbox.B = 0.0;
    }

    /* select by coordinate */
    if (coord_opt->answer) 
        line = Vect_find_line (Map, east, north, 0.0,
			      GV_POINT | GV_CENTROID | GV_LINE | GV_BOUNDARY | GV_FACE,
			       maxdist, 0, 0);

    /* select by bounding box */
    if (line == 0) {

        G_debug(1, "Vect_find_line found nothing, trying bbox: n=%f s=%f w=%f e=%f\n",
		bbox.N, bbox.S, bbox.W, bbox.E);

        Vect_select_lines_by_box (Map, &bbox,
				  GV_POINTS | GV_LINES | GV_BOUNDARY | GV_CENTROID | GV_FACE,
				  List);

        /* get the only first one */
        if (List->n_values < 1)
            line = 0;
        else 
            line = List->value[0];
    }


    G_debug (2, "line = %d", line);


    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();
    
    type = Vect_read_line(Map, Points, Cats, line);

    cat = Vect_get_line_cat (Map, line, field);

    if (cat > 0) 
      sprintf(buff,_("category [%d]"),cat);
        
    G_debug(2, "Moving type [%d] number [%d] %s", type, line, buff);

    /* move */
    for (j = 0; j < Points->n_points; j++) {
        if ((bbox.W <= Points->x[j] && Points->x[j] <= bbox.E) && 
            (bbox.S <= Points->y[j] && Points->y[j] <= bbox.N)){
            Points->x[j]+=move[0];
            Points->y[j]+=move[1];
            res++;
        }
    } /* for each point at line */

    if (Vect_rewrite_line (Map, line, type, Points, Cats) < 0)  {
	G_warning("Feature could not be moved");
        return -1;
    }

    if (i_flg->answer) 
	fprintf(stdout,"%d\n", line);

    G_message(_("[%d] vertexes moved"), res);

    return res;
}

int do_break(struct Map_info *Map)
{
    int res;
    double east, north;
    int line, j;
    double maxdist;
    char buff[16] = "";

    int cat;
    int type;
    int field;

    int seg;
    double px, py;

    struct line_pnts *Points,*NPoints;
    struct line_cats *Cats;

    res = seg = 0;
    maxdist = 0.0;

    field   = atoi(fld_opt->answer);
    east    = atof(coord_opt->answers[0]);
    north   = atof(coord_opt->answers[1]);
    maxdist = max_distance(atof(maxdist_opt->answer));

    line = Vect_find_line(Map, east, north, 0.0,
			  GV_LINE | GV_BOUNDARY | GV_FACE,
			  maxdist, 0, 0);


    G_debug (2, "line = %d", line);

    if (line > 1) {
	Points  = Vect_new_line_struct();
	NPoints = Vect_new_line_struct();
	Cats    = Vect_new_cats_struct();
        
        type = Vect_read_line(Map, Points, Cats, line);

	cat = Vect_get_line_cat(Map, line, field);

        if (cat > 0) 
            sprintf(buff,"category [%d]",cat);

        G_debug(2, "Spliting type [%d] number [%d] %s", type, line, buff);

        seg = Vect_line_distance ( Points, east, north, 0, 0,
				   &px, &py, NULL, NULL, NULL, NULL);

        /* copy first line part */
        for (j = 0; j < seg; j++) {
            Vect_append_point(NPoints, Points->x[j],Points->y[j], Points->z[j]);
        }

        /* add split vertex */
        Vect_append_point(NPoints, east, north, 0.0);

        /* copy second line part */
        for (j = seg; j < Points->n_points; j++) {
            Vect_append_point(NPoints, Points->x[j], Points->y[j], Points->z[j]);
        }

        /* rewrite the line */
        if (Vect_rewrite_line (Map, line, type, NPoints, Cats) < 0) {
            G_warning("Line could not be split");
            return -1;
        }
    }

    if (i_flg->answer) 
        fprintf(stdout,"%d\n", line);

    G_message(_("Line [%d] broken"), line);

    return 1;
}

int do_remove_vertex(struct Map_info *Map)
{
    double east, north,xo,yo, dist;
    int line,j;
    double maxdist = 0.;
    char buff[16] = "";

    int type;
    struct line_pnts *Points,*NPoints;
    struct line_cats *Cats;
    int cat;
    int field=atoi(fld_opt->answer);
    int seg = 0;

    east = atof(coord_opt->answers[0]);
    north = atof(coord_opt->answers[1]);
    maxdist =  max_distance(atof(maxdist_opt->answer));

    line = Vect_find_line(Map, east, north, 0.0, GV_LINE | GV_BOUNDARY | GV_FACE, maxdist, 0, 0);

    G_debug (2, "line = %d", line);

    if (line > 1) {
	Points = Vect_new_line_struct();
	NPoints = Vect_new_line_struct();
	Cats = Vect_new_cats_struct();
        Vect_reset_line(NPoints);
        type = Vect_read_line(Map, Points, Cats, line);
        if ((cat = Vect_get_line_cat(Map, line, field )) > 0) 
            sprintf(buff,"category [%d]",cat);

            
        G_debug(2, "Spliting type [%d] number [%d] %s", type, line, buff);

        /* find nearest vertex */

        seg = Vect_line_distance ( Points, east, north, 0, 0, &xo, &yo, NULL, NULL, NULL, NULL );
        dist = Vect_points_distance ( xo, yo, 0, Points->x[seg-1], Points->y[seg-1], 0, 0);

        if ( dist < Vect_points_distance ( xo, yo, 0, Points->x[seg], Points->y[seg], 0, 0) ) {
            seg -= 1;
        }
        xo = Points->x[seg];
        yo = Points->y[seg];

        /* remove vertex */
        for (j = 0; j < Points->n_points; j++) {

            if (xo == Points->x[j] && yo == Points->y[j]) 
                continue;
            Vect_append_point(NPoints,Points->x[j],Points->y[j], Points->z[j]);

        }

        /* rewrite the line */
        if ( Vect_rewrite_line (Map, line, type, NPoints, Cats) < 0)  {
            G_warning("Vertex could not be removed");
            return -1;
        }
        /* attr_del(Map, layer, cat);*/
    }

    if (i_flg->answer) 
        fprintf(stdout,"%d\n", line);
    G_message(_("Vertex on line [%d] removed"), line);

    return 1;
}

int do_split(struct Map_info *Map)
{
    double east, north;
    int line,j;
    double maxdist = 0.;
    char buff[16] = "";

    int type;
    struct line_pnts *Points,*NPoints;
    struct line_cats *Cats;
    int cat;
    int field=atoi(fld_opt->answer);
    int seg = 0;
    double px,py;

    east = atof(coord_opt->answers[0]);
    north = atof(coord_opt->answers[1]);
    maxdist =  max_distance(atof(maxdist_opt->answer));

    line = Vect_find_line(Map, east, north, 0.0, GV_LINE | GV_BOUNDARY | GV_FACE, maxdist, 0, 0);


    G_debug (2, "line = %d", line);

    if (line > 1) {
	Points = Vect_new_line_struct();
	NPoints = Vect_new_line_struct();
	Cats = Vect_new_cats_struct();
        Vect_reset_line(NPoints);
        type = Vect_read_line(Map, Points, Cats, line);
        if ((cat = Vect_get_line_cat(Map, line, field )) > 0) 
            sprintf(buff,"category [%d]",cat);

        G_debug(2, "Spliting type [%d] number [%d] %s", type, line, buff);

        seg = Vect_line_distance ( Points, east, north, 0, 0, &px, &py, NULL, NULL, NULL, NULL );

        /* copy first line part */
        for (j = 0; j < seg; j++) {

            Vect_append_point(NPoints,Points->x[j],Points->y[j], Points->z[j]);

        }

        /* add last vertex */
        Vect_append_point(NPoints,east,north,0.0);

        /* rewrite the line */
        if ( Vect_rewrite_line (Map, line, type, NPoints, Cats) < 0)  {
            G_warning("Line could not be broken");
            return -1;
        }
        /* attr_del(Map, layer, cat);*/

        Vect_reset_line(NPoints);

        /* add last vertex */
        Vect_append_point(NPoints,east,north,0.0);

        /* copy second line part */
        for (j = seg; j < Points->n_points; j++) {

            Vect_append_point(NPoints,Points->x[j],Points->y[j], Points->z[j]);

        }

        /* rewrite the line */
        if ( Vect_write_line (Map, type, NPoints, Cats) < 0)  {
            G_warning("Line could not be split");
            return -1;
        }
    }

    if (i_flg->answer) 
        fprintf(stdout,"%d\n", line);
    G_message(_("Line [%d] split"), line);

    return 1;
}
