/****************************************************************
 *
 * MODULE:       v.buffer
 * 
 * AUTHOR(S):    Radim Blazek
 *               
 * PURPOSE:      Vector buffer
 *               
 * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 **************************************************************/
#include <stdlib.h> 
#include <string.h> 
#include "gis.h"
#include "Vect.h"

/* returns 1 if point is within buffer */
int inside_buffer ( struct Map_info *In, int type, double buffer, double x, double y ) 
{
    int i;
    static struct ilist *List = NULL;
    static struct line_pnts *Points = NULL;

    BOUND_BOX box;

    if ( List == NULL ) 
	List = Vect_new_list ();
    else
	Vect_reset_list ( List );

    if ( Points == NULL ) 
	Points = Vect_new_line_struct ();


    /* Inside area ? */
    if ( type & GV_AREA) {
	int area, centroid;
	/* inside */
	area = Vect_find_area ( In, x, y );
	centroid = 0;
	if ( area )
	    centroid = Vect_get_area_centroid ( In, area );

	G_debug ( 3, "    area = %d centroid = %d", area, centroid );
	if ( centroid ) 
	    return 1;
    }

    /* ouside area, within buffer? */
    /* The centroid is in buffer if at least one point/line/boundary is in buffer distance */
    box.E = x + buffer; box.W = x - buffer;
    box.N = y + buffer; box.S = y - buffer;
    box.T = PORT_DOUBLE_MAX; box.B = -PORT_DOUBLE_MAX;
    
    Vect_select_lines_by_box ( In, &box, GV_POINTS | GV_LINES, List );
    G_debug ( 3, "  %d lines selected by box", List->n_values );

    for ( i = 0; i < List->n_values; i++) {
	int line, ltype;
	double dist;
	line = List->value[i];
    
	G_debug ( 3, "    line[%d] = %d", i, line );

	ltype = Vect_read_line (In, Points, NULL, line);
	
	Vect_line_distance ( Points, x, y, 0, 0, NULL, NULL, NULL, &dist, NULL, NULL);
	G_debug ( 3, "    dist = %f", dist );
	if ( dist > buffer ) continue;
	
	/* lines */
	if ( type & ltype ) 
	    return 1;

	/* Areas */ 
	if ( (type & GV_AREA) && ltype == GV_BOUNDARY ) {
	    int j, side[2], centr[2], area_in;
	    
	    Vect_get_line_areas ( In, line, &side[0], &side[1] );

	    for ( j = 0; j < 2; j++ ) { 
		centr[j] = 0;

		if ( side[j] > 0 )
		    area_in = side[j];
		else /* island */
		    area_in = Vect_get_isle_area ( In, abs ( side[j] ) );

		if ( area_in > 0 )
		    centr[j] = Vect_get_area_centroid ( In, area_in );
	    }
	
	    if ( centr[0] || centr[1] ) 
		return 1;
	}	    
    }
    return 0;
}

int 
main (int argc, char *argv[])
{
    struct Map_info In, Out;
    struct line_pnts *Points, *BPoints;
    struct line_cats *Cats;
    char   *mapset;
    struct GModule *module;
    struct Option *in_opt, *out_opt, *type_opt, *buffer_opt, *tolerance_opt;
    struct Flag *b_flag;
    double buffer, tolerance;
    int    type;

    module = G_define_module();
    module->description = "Create a buffer around features of given type (areas must contain centroid).";

    in_opt = G_define_standard_option(G_OPT_V_INPUT);
    out_opt = G_define_standard_option(G_OPT_V_OUTPUT);
    
    type_opt = G_define_standard_option(G_OPT_V_TYPE) ;
    type_opt->options = "point,line,boundary,centroid,area";
    type_opt->answer = "point,line,area";

    buffer_opt = G_define_option();
    buffer_opt->key = "buffer";
    buffer_opt->type = TYPE_DOUBLE;
    buffer_opt->required = YES;
    buffer_opt->description = "Buffer distance";
	
    tolerance_opt = G_define_option();
    tolerance_opt->key = "tolerance";
    tolerance_opt->type = TYPE_DOUBLE;
    tolerance_opt->required = NO;
    tolerance_opt->answer = "1";
    tolerance_opt->description = "Maximum distance between theoretical arc and polygon segments.";

    b_flag = G_define_flag ();
    b_flag->key             = 'b';
    b_flag->description     = "Output raw boundaries only (debugging).";

    G_gisinit(argv[0]);
    if (G_parser (argc, argv))
	exit(-1); 
    
    type = Vect_option_to_types ( type_opt );
    buffer = abs ( atof( buffer_opt->answer ) );
    tolerance = atof( tolerance_opt->answer );

    Points = Vect_new_line_struct ();
    BPoints = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    /* open input vector */
    if ((mapset = G_find_vector2 (in_opt->answer, "")) == NULL) {
	 G_fatal_error ( "Could not find input map <%s>\n", in_opt->answer);
    }
    
    Vect_set_open_level (2); 
    Vect_open_old (&In, in_opt->answer, mapset); 

    Vect_set_fatal_error (GV_FATAL_PRINT);
    if (0 > Vect_open_new (&Out, out_opt->answer, 0) ) {
	 Vect_close (&In);
	 exit (1);
    }

    Vect_copy_head_data (&In, &Out);
    Vect_hist_copy (&In, &Out);
    Vect_hist_command ( &Out );

    /* Create buffers' boundaries */

    /* Lines */
    if ( (type & GV_POINTS) || (type & GV_LINES) ) {
	int nlines, line, ltype;
	nlines = Vect_get_num_lines ( &In );
	for ( line = 1; line <= nlines; line++ ) {
	    G_debug ( 3, "line = %d", line );
	    ltype = Vect_read_line (&In, Points, NULL, line);
	    if ( !(ltype & type ) ) continue;

	    Vect_line_buffer ( Points, buffer, tolerance, BPoints );	
	    Vect_write_line ( &Out, GV_BOUNDARY, BPoints, Cats );  
	}
    }

    /* Areas */
    if ( type & GV_AREA ) {
	int i, nareas, area, centroid, nisles, isle;
	
	nareas = Vect_get_num_areas ( &In );
	for ( area = 1; area <= nareas; area++ ) {
	    centroid = Vect_get_area_centroid ( &In, area );
	    if ( centroid == 0 ) continue;
	    
	    /* outer ring */
	    Vect_get_area_points ( &In, area, Points );
	    Vect_line_buffer ( Points, buffer, tolerance, BPoints );	
	    Vect_write_line ( &Out, GV_BOUNDARY, BPoints, Cats );  
	    
	    /* islands */
	    nisles = Vect_get_area_num_isles (&In, area);
	    for ( i = 0; i < nisles; i++ ) {
		isle = Vect_get_area_isle (&In, area, i);
		Vect_get_isle_points ( &In, isle, Points );
		
		Vect_line_buffer ( Points, buffer, tolerance, BPoints );	
		Vect_write_line ( &Out, GV_BOUNDARY, BPoints, Cats );  
	    }
	}
    }

    /* Create areas */
    if ( !b_flag->answer ) {
	int ret, nareas, area, nlines, line;
	double x, y;
	char *Areas, *Lines;
	
	/* Break lines */
	fprintf (stderr, "Building parts of topology ...\n" );
	Vect_build_partial ( &Out, GV_BUILD_BASE, stderr );

	fprintf ( stderr, "Breaking boundaries ...\n" );
	Vect_break_lines ( &Out, GV_BOUNDARY, NULL, stderr );

        fprintf ( stderr, "Removing duplicates ...\n" );
        Vect_remove_duplicates ( &Out, GV_BOUNDARY, NULL, stderr );

        fprintf ( stderr, "Attaching islands ...\n" );
        Vect_build_partial ( &Out, GV_BUILD_ATTACH_ISLES, stderr );

        /* Calculate new centroids for all areas */
        nareas = Vect_get_num_areas ( &Out );
	Areas = (char *) G_calloc ( nareas+1, sizeof(char) );
        for ( area = 1; area <= nareas; area++ ) {
	    G_debug ( 3, "area = %d", area );
	    
            ret = Vect_get_point_in_area ( &Out, area, &x, &y );
            if ( ret < 0 ) {
		G_warning ("Cannot calculate area centroid" );
		continue;
	    }

	    if ( inside_buffer ( &In, type, buffer, x, y ) ) 
	       Areas[area] = 1;	
	}

	/* Make a list of boundaries to be deleted (both sides inside) */
	nlines = Vect_get_num_lines ( &Out );
	G_debug ( 3, "nlines = %d", nlines );
	Lines = (char *) G_calloc ( nlines+1, sizeof(char) );

	for ( line = 1; line <= nlines; line++ ) {
            int j, side[2], areas[2];
	    
	    G_debug ( 3, "line = %d", line );

	    if ( !Vect_line_alive ( &Out, line ) ) continue;
	    
	    Vect_get_line_areas ( &Out, line, &side[0], &side[1] );

	    for ( j = 0; j < 2; j++ ) { 
		if ( side[j] > 0 ) {
		    areas[j] = side[j];
	        } else { /* island */
		    areas[j] = Vect_get_isle_area ( &Out, abs ( side[j] ) );
		}
	    }
	
	    G_debug ( 3, " areas = %d , %d -> Areas = %d, %d", areas[0], areas[1], 
		                    Areas[areas[0]], Areas[areas[1]] );
	    if ( Areas[areas[0]] && Areas[areas[1]] ) 
		Lines[line] = 1;
	}
        G_free( Areas );
	
	/* Delete boundaries */
	for ( line = 1; line <= nlines; line++ ) {
	    if ( Lines[line] ) {
	        G_debug ( 3, " delete line %d", line );
		Vect_delete_line ( &Out, line );
	    }
	}

        G_free( Lines );

        /* Create new centroids */
        Vect_cat_set (Cats, 1, 1 );
        nareas = Vect_get_num_areas ( &Out );
        for ( area = 1; area <= nareas; area++ ) {
	    G_debug ( 3, "area = %d", area );

	    if ( !Vect_area_alive ( &Out, area ) ) continue;
	    
            ret = Vect_get_point_in_area ( &Out, area, &x, &y );
            if ( ret < 0 ) {
		G_warning ("Cannot calculate area centroid" );
		continue;
	    }

	    if ( inside_buffer ( &In, type, buffer, x, y ) ) {
		Vect_reset_line ( Points );
		Vect_append_point ( Points, x, y, 0 );
		Vect_write_line ( &Out, GV_CENTROID, Points, Cats );
	    }
	}

        fprintf ( stderr, "Attaching centroids ...\n" );
        Vect_build_partial ( &Out, GV_BUILD_CENTROIDS, stderr );
    }
    
    Vect_close (&In);

    fprintf ( stderr, "Rebuilding topology ...\n" );
    Vect_build_partial ( &Out, GV_BUILD_NONE, NULL );
    Vect_build (&Out, stderr);
    Vect_close (&Out);

    exit(0) ;
}


