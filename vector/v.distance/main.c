/***************************************************************
 *
 * MODULE:       v.distance
 * 
 * AUTHOR(S):    - J.Soimasuo 15.9.1994, University of Joensuu,
 *                 Faculty of Forestry, Finland
 *               - some additions 2002 Markus Neteler
 *               - updated to 5.7 by Radim Blazek 2003
 *               
 * PURPOSE:      Calculates distance from a point to nearest line or point in vector layer. 
 *               
 * COPYRIGHT:    (C) 2002 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 **************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"

/* TODO: support all types (lines, boundaries, areas for 'from' (from_type) */

/* define codes for characteristics of relation between two nearest features */
#define CAT        1 /* category of nearest feature */
#define FROM_X     2 /* x coordinate of nearest point on 'from' feature */
#define FROM_Y     3 /* y coordinate of nearest point on 'from' feature */
#define TO_X       4 /* x coordinate of nearest point on 'to' feature */
#define TO_Y       5 /* y coordinate of nearest point on 'to' feature */
#define FROM_ALONG 6 /* distance to nearest point on 'from' along linear feature */
#define TO_ALONG   7 /* distance to nearest point on 'to' along linear feature */
#define DIST       8 /* minimum distance to nearest feature */

#define END        9 /* end of list */

/* Structure where are stored infos about nearest feature for each category */
typedef struct {
    int    from_cat;     /* category (from) */
    int    count;    /* number of features already found */ 
    int    to_cat;  /* category (to) */
    double from_x, from_y, to_x, to_y;    /* coordinates of nearest point */
    double from_along, to_along;    /* distance along a linear feature to the nearest point */
    double dist;  /* distance to nearest feature */
} NEAR;

/* Upload and column store */
typedef struct {
    int  upload;   /* code */
    char *column;   /* column name */
} UPLOAD;

int cmp_near ( const void *, const void *);
int cmp_exist ( const void *, const void *);

int main (int argc, char *argv[])
{
    int    i, j;
    int    all = 0; /* calculate from each to each within the threshold */
    char   *mapset;
    struct GModule *module;
    struct Option *from_opt, *to_opt, *from_type_opt, *to_type_opt, *from_field_opt, *to_field_opt;
    struct Option *out_opt, *max_opt, *table_opt;
    struct Option *upload_opt, *column_opt;
    struct Flag *print_flag, *all_flag;
    struct Map_info From, To, Out, *Outp;
    int    from_type, to_type, from_field, to_field;
    double max;
    struct line_pnts *FPoints, *TPoints;
    struct line_cats *FCats, *TCats;
    NEAR   *Near, *near;
    UPLOAD *Upload; /* zero terminated */
    int ftype, fcat, tcat, count;
    int nfrom, nto, nfcats, fline, tline, tseg, tarea, area, isle, nisles;
    double tx, ty, dist, talong, tmp_tx, tmp_ty, tmp_dist, tmp_talong;
    struct field_info *Fi;
    dbString stmt;
    dbDriver *driver;
    int  *catexist, ncatexist, *cex; 
    char buf1[2000], buf2[2000];
    int update_ok, update_err, update_exist, update_notexist, update_dupl, update_notfound;
    struct ilist *List;
    BOUND_BOX box;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description = "Find the nearest element in vector 'to' for elements in vector 'from'. "
            "Various information about this relation may be uploaded to the attribute table of "
	    "input vector 'from' or printed to stdout.";

    from_opt = G_define_standard_option(G_OPT_V_INPUT);
    from_opt->key         = "from" ;
    from_opt->description = "Name of existing vector file.";

    to_opt = G_define_standard_option(G_OPT_V_INPUT);
    to_opt->key         = "to" ;
    to_opt->description = "Name of existing vector file.";

    from_type_opt = G_define_standard_option(G_OPT_V_TYPE); 
    from_type_opt->key         = "from_type";
    from_type_opt->options     = "point,centroid";
    from_type_opt->answer      = "point";
    from_type_opt->description = "From type";

    to_type_opt = G_define_standard_option(G_OPT_V_TYPE); 
    to_type_opt->key         = "to_type";
    to_type_opt->options     = "point,line,boundary,centroid,area";
    to_type_opt->answer      = "point,line,area";
    to_type_opt->description = "To type";

    from_field_opt = G_define_standard_option(G_OPT_V_FIELD) ;     
    from_field_opt->key         = "from_layer";
    from_field_opt->description = "From layer";
    
    to_field_opt = G_define_standard_option(G_OPT_V_FIELD) ;     
    to_field_opt->key         = "to_layer";
    to_field_opt->description = "To layer";
    
    out_opt = G_define_standard_option(G_OPT_V_OUTPUT);
    out_opt->key         = "output";
    out_opt->required    = NO;
    out_opt->description = "New vector map containing lines connecting nearest elements";
    
    max_opt = G_define_option();
    max_opt->key = "dmax";
    max_opt->type = TYPE_DOUBLE;
    max_opt->required = NO;
    max_opt->answer = "-1";
    max_opt->description = "Maximum distance or -1 for no limit.";

    upload_opt = G_define_option();
    upload_opt->key = "upload";
    upload_opt->type = TYPE_STRING;
    upload_opt->required = YES;
    upload_opt->multiple = YES;
    upload_opt->options = "cat,dist,to_x,to_y,to_along";
    upload_opt->description = "Values describing the relation between two nearest features:\n"
	"\tcat - category of the nearest feature\n"
	"\tdist - minimum distance to nearest feature\n"
     /*	"\tfrom_x - x coordinate of the nearest point on 'from' feature\n" */
     /*	"\tfrom_y - y coordinate of the nearest point on 'from' feature\n" */
	"\tto_x - x coordinate of the nearest point on 'to' feature\n"
	"\tto_y - y coordinate of the nearest point on 'to' feature\n"
     /* "\tfrom_along - distance to the nearest point on 'from' feature along linear feature\n" */
	"\tto_along - distance to the nearest point on 'from' feature along linear feature";

    column_opt = G_define_option();
    column_opt->key = "column";
    column_opt->type = TYPE_STRING;
    column_opt->required = YES;
    column_opt->multiple = YES;
    column_opt->description = "Column name(s) where values specified by 'upload' option will be uploaded";

    table_opt = G_define_option();
    table_opt->key = "table";
    table_opt->type = TYPE_STRING;
    table_opt->required = NO;
    table_opt->multiple = NO;
    table_opt->description = "The name of the table created for output when -a flag is used";

    print_flag = G_define_flag();
    print_flag->key = 'p';
    print_flag->description = "Print output to stdout, don't update attribute table. "
	    "First column is always category of 'from' feature called from_cat.";
    
    all_flag = G_define_flag();
    all_flag->key = 'a';
    all_flag->description = "Calculate distances to all features within the threshold. "
      "The output is written to stdout but may be uploaded to a new table created by this module. "
      "From categories are may be multiple.";
    
    if (G_parser(argc, argv)) exit(-1);
   
    from_type = Vect_option_to_types ( from_type_opt );
    to_type = Vect_option_to_types ( to_type_opt );
    
    from_field = atoi ( from_field_opt->answer); 
    to_field = atoi ( to_field_opt->answer); 
    
    max = atof (max_opt->answer);

    /* Read upload and column options */
    /* count */
    i = 0;
    while (upload_opt->answers[i]) i++;
    /* alloc */
    Upload = (UPLOAD *) G_calloc ( i+1, sizeof (UPLOAD) );
    /* read upload */
    i = 0;
    while (upload_opt->answers[i]) {
	if ( strcmp(upload_opt->answers[i], "cat") == 0 )
	    Upload[i].upload = CAT;
	else if ( strcmp(upload_opt->answers[i], "from_x") == 0 )
	    Upload[i].upload = FROM_X;
	else if ( strcmp(upload_opt->answers[i], "from_y") == 0 )
	    Upload[i].upload = FROM_Y;
	else if ( strcmp(upload_opt->answers[i], "to_x") == 0 )
	    Upload[i].upload = TO_X;
	else if ( strcmp(upload_opt->answers[i], "to_y") == 0 )
	    Upload[i].upload = TO_Y;
	else if ( strcmp(upload_opt->answers[i], "from_along") == 0 )
	    Upload[i].upload = FROM_ALONG;
	else if ( strcmp(upload_opt->answers[i], "to_along") == 0 )
	    Upload[i].upload = TO_ALONG;
	else if ( strcmp(upload_opt->answers[i], "dist") == 0 )
	    Upload[i].upload = DIST;

	i++;
    }
    Upload[i].upload = END;
    /* read columns */
    i = 0;
    while (column_opt->answers[i]) {
	if ( Upload[i].upload == END ) {
	    G_warning ("Too many column names");
	    break;
	}
	Upload[i].column = G_store ( column_opt->answers[i] );
	i++;
    }
    if ( Upload[i].upload != END ) 
	G_fatal_error ("Not enough column names");
    
    if ( all_flag->answer ) all = 1;

    /* Open 'from' vector */
    mapset = G_find_vector2 (from_opt->answer, "");
    
    if ( !print_flag->answer && strcmp(mapset,G_mapset()) != 0 )
       G_fatal_error ( "Vector 'from' is not in user mapset and cannot be updated.");

    Vect_set_open_level (2);
    Vect_open_old (&From, from_opt->answer, mapset);

    /* Open 'to' vector */
    mapset = G_find_vector2 (to_opt->answer, "");
    Vect_set_open_level (2);
    Vect_open_old (&To, to_opt->answer, mapset);

    /* Open output vector */
    if ( out_opt->answer ) {
        Vect_open_new (&Out, out_opt->answer, WITHOUT_Z);
	Vect_hist_command ( &Out );
	Outp = &Out;
    } else {
	Outp = NULL;
    }

    /* TODO: add maxdist = -1 to Vect_select_ !!! */
    /* Calc maxdist */
    if ( max < 0 ) {
        BOUND_BOX fbox, tbox;
	double dx, dy;

	Vect_get_map_box ( &From, &fbox );
	Vect_get_map_box ( &To, &tbox );

	Vect_box_extend ( &fbox, &tbox );

	dx = fbox.E - fbox.W;
	dy = fbox.N - fbox.S;

	max = sqrt ( dx*dx + dy*dy );

	G_debug ( 2, "max = %f", max );
    }

    /* Open database driver */
    driver = NULL;
    if ( !print_flag->answer ) { 
        db_init_string (&stmt);

	if (!all) {
	    Fi = Vect_get_field ( &From, from_field);
	    if ( Fi == NULL ) G_fatal_error ( "Cannot get layer info" );
	    
	    driver = db_start_driver_open_database ( Fi->driver, Fi->database );
	    if ( driver == NULL ) 
		G_fatal_error ( "Cannot open database %s by driver %s", Fi->database, Fi->driver );
	} else {
	    driver = db_start_driver_open_database ( NULL, NULL );
	    if ( driver == NULL ) 
		G_fatal_error ( "Cannot open default database");
	}
    } 

    FPoints=Vect_new_line_struct();
    TPoints=Vect_new_line_struct();
    FCats = Vect_new_cats_struct ();
    TCats = Vect_new_cats_struct ();
    List = Vect_new_list ();
    
    /* Allocate space ( may be more than needed (duplicate cats and elements without cats) ) */
    nfrom = Vect_get_num_lines ( &From );
    nto = Vect_get_num_lines ( &To );
    if ( all ) {
        Near = (NEAR *) G_calloc ( nfrom * nto , sizeof (NEAR) );
    } else {
        Near = (NEAR *) G_calloc ( nfrom, sizeof (NEAR) );
    }

    /* Read all cats from 'from' */
    if ( !all ) {
	nfcats = 0;
	for ( i = 1; i <= nfrom; i++ ) {
	    ftype = Vect_read_line ( &From, NULL, FCats, i );

	    /* This keeps also categories of areas for future (if area s in from_type) */
	    if ( !(ftype & from_type) && (ftype != GV_CENTROID || !(from_type & GV_AREA)) ) continue;
	    
	    Vect_cat_get ( FCats, from_field, &fcat );
	    if ( fcat < 0 ) continue;
	    Near[nfcats].from_cat = fcat; 
	    nfcats++;
	}
	G_debug (1, "%d cats loaded from vector (including duplicates)", nfcats);
	/* Sort by cats and remove duplicates */
	qsort( (void *)Near, nfcats, sizeof(NEAR), cmp_near);

	/* remove duplicates */
	for ( i = 1; i < nfcats; i++ ) {
	    if ( Near[i].from_cat == Near[i-1].from_cat ) {
		for ( j = i; j < nfcats - 1; j++ ) {
		    Near[j].from_cat = Near[j+1].from_cat;
		}
		nfcats--;
	    }
	}
	
	G_debug (1, "%d cats loaded from vector (unique)", nfcats);
    }

    /* Go through all lines in 'from' and find nearest in 'to' for each */
    /* Note: as from_type is restricted to GV_POINTS (for now) everything is simple */
    
    count = 0; /* count of distances in 'all' mode */
    /* Find nearest lines */
    if ( to_type & (GV_POINTS | GV_LINES) ) {
	for ( fline = 1; fline <= nfrom ; fline++ ) {
	    int tmp_tcat;

	    G_debug (3, "fline = %d", fline);
	    ftype = Vect_read_line ( &From, FPoints, FCats, fline );
	    if ( !(ftype & from_type) ) continue;

	    Vect_cat_get ( FCats, from_field, &fcat );
	    if ( fcat < 0 && !all ) continue;
	    
	    box.E = FPoints->x[0] + max ; box.W = FPoints->x[0] - max ; 
	    box.N = FPoints->y[0] + max ; box.S = FPoints->y[0] - max;
	    box.T = PORT_DOUBLE_MAX;      box.B = -PORT_DOUBLE_MAX;

	    Vect_select_lines_by_box ( &To, &box, to_type, List);
	    G_debug (3, "  %d lines in box", List->n_values);

	    tline = 0;
	    for (i = 0; i < List->n_values; i++) {
		Vect_read_line ( &To, TPoints, TCats, List->value[i] );

		tseg = Vect_line_distance ( TPoints, FPoints->x[0], FPoints->y[0], 0, 0, 
				   &tmp_tx, &tmp_ty, NULL, &tmp_dist, NULL, &tmp_talong);

		if ( tmp_dist > max ) continue; /* not in threshold */
		
		/* TODO: more cats of the same field */
		Vect_cat_get(TCats, to_field, &tmp_tcat);
	        G_debug (4, "  tmp_dist = %f tmp_tcat = %d", tmp_dist, tmp_tcat);

		if ( all ) {
		    /* find near by cat */ 
		    near = &(Near[count]); 

		    /* store info about relation */
		    near->from_cat = fcat;
		    near->to_cat = tmp_tcat;  /* -1 is OK */
		    near->dist = tmp_dist;
		    near->from_x = FPoints->x[0];
		    near->from_y = FPoints->y[0];
		    near->to_x = tmp_tx;
		    near->to_y = tmp_ty;    
		    near->to_along = tmp_talong; /* 0 for points */
		    near->count++;
		    count++;
		} else { 
		    if ( tline == 0 || (tmp_dist <= dist)) {
			tline =  List->value[i];
			tcat = tmp_tcat;
			dist = tmp_dist;
			tx = tmp_tx;
			ty = tmp_ty;
			talong = tmp_talong;
		    }
		}
	    }

	    G_debug (4, "  dist = %f", dist );
	    if ( !all && tline > 0 ) {
		/* find near by cat */ 
		near = (NEAR *) bsearch((void *) &fcat, Near, nfcats, sizeof(NEAR), cmp_near); 

		G_debug (4, "  near.from_cat = %d near.count = %d", near->from_cat, near->count);
		/* store info about relation */
		if ( near->count == 0 || near->dist > dist ) {
		    near->to_cat = tcat;  /* -1 is OK */
		    near->dist = dist;
		    near->from_x = FPoints->x[0];
		    near->from_y = FPoints->y[0];
		    near->to_x = tx;
		    near->to_y = ty;    
		    near->to_along = talong; /* 0 for points */
		}
		near->count++;
	    }
	}
    }

    /* Find nearest areas */
    if ( to_type & GV_AREA ) {
	for ( fline = 1; fline <= nfrom ; fline++ ) {
	    G_debug (3, "fline = %d", fline);
	    ftype = Vect_read_line ( &From, FPoints, FCats, fline );
	    if ( !(ftype & from_type) ) continue;

	    Vect_cat_get ( FCats, from_field, &fcat );
	    if ( fcat < 0 && !all ) continue;
	    

	    /* select areas by box */
	    box.E = FPoints->x[0] + max ; box.W = FPoints->x[0] - max ; 
	    box.N = FPoints->y[0] + max ; box.S = FPoints->y[0] - max;
	    box.T = PORT_DOUBLE_MAX;      box.B = -PORT_DOUBLE_MAX;

	    Vect_select_areas_by_box ( &To, &box, List);
	    G_debug ( 4, "%d areas selected by box", List->n_values );

	    /* For each area in box check the distance */
	    tarea = 0;
	    dist = PORT_DOUBLE_MAX;
	    for (i = 0; i < List->n_values; i++) {
		int tmp_tcat;

		area = List->value[i];
		G_debug ( 4, "%d: area %d", i, area );
		Vect_get_area_points ( &To, area, TPoints);

		/* Find the distance to this area */
		if ( Vect_point_in_area ( &To, area, FPoints->x[0], FPoints->y[0] ) ) { /* in area */
		    tmp_dist = 0;
		    tmp_tx = FPoints->x[0];
		    tmp_ty = FPoints->y[0];
		} else if (  Vect_point_in_poly (FPoints->x[0], FPoints->y[0], TPoints) > 0) { /* in isle */
		    nisles = Vect_get_area_num_isles ( &To, area );
		    for ( j = 0; j < nisles; j++ ) {
			double tmp2_dist, tmp2_tx, tmp2_ty; 

			isle = Vect_get_area_isle ( &To, area, j );
			Vect_get_isle_points ( &To, isle, TPoints);
			Vect_line_distance ( TPoints, FPoints->x[0], FPoints->y[0], 0, 0, 
				   &tmp2_tx, &tmp2_ty, NULL, &tmp2_dist, NULL, NULL);

			if ( j == 0 || tmp2_dist < tmp_dist ) {
			    tmp_dist = tmp2_dist;
			    tmp_tx = tmp2_tx;
			    tmp_ty = tmp2_ty;
			}
		    }
		} else { /* outside area */
		    Vect_line_distance ( TPoints, FPoints->x[0], FPoints->y[0], 0, 0, 
			       &tmp_tx, &tmp_ty, NULL, &tmp_dist, NULL, NULL);

		}
	        if ( tmp_dist > max ) continue; /* not in threshold */
		Vect_get_area_cats ( &To, area, TCats );
		tmp_tcat = -1;
		/* TODO: all cats of given field ? */
		for ( j = 0; j < TCats->n_cats; j++) {
		    if ( TCats->field[j] == to_field ) {
			if ( tmp_tcat >= 0 ) 
			    G_warning ( "more cats of to_layer" );
			tmp_tcat = TCats->cat[j];
		    }
		}

	        G_debug (4, "  tmp_dist = %f tmp_tcat = %d", tmp_dist, tmp_tcat);

		if ( all ) {
		    /* find near by cat */ 
		    near = &(Near[count]); 

		    /* store info about relation */
		    near->from_cat = fcat;
		    near->to_cat = tmp_tcat;  /* -1 is OK */
		    near->dist = tmp_dist;
		    near->from_x = FPoints->x[0];
		    near->from_y = FPoints->y[0];
		    near->to_x = tmp_tx;
		    near->to_y = tmp_ty;    
		    near->to_along = 0; /* nonsense for areas */
		    near->count++;
		    count++;
		} else if ( tarea == 0 || tmp_dist < dist ) {
		    tarea = area;
		    tcat = tmp_tcat;
		    dist = tmp_dist;
		    tx = tmp_tx;
		    ty = tmp_ty;
		}
	    }

	    if ( !all && tarea > 0 ) {
		/* find near by cat */ 
		near = (NEAR *) bsearch((void *) &fcat, Near, nfcats, sizeof(NEAR), cmp_near); 
		G_debug (4, "near.from_cat = %d near.count = %d dist = %f", 
			                   near->from_cat, near->count, near->dist);

		/* store info about relation */
		if ( near->count == 0 || near->dist > dist ) {
		    near->to_cat = tcat;  /* -1 is OK */
		    near->dist = dist;
		    near->from_x = FPoints->x[0];
		    near->from_y = FPoints->y[0];
		    near->to_x = tx;
		    near->to_y = ty;    
		    near->to_along = 0; /* nonsense for areas */
		}
		near->count++;
	    }
	}
    }

    G_debug (3, "count = %d", count);

    /* Update database / print to stdout / create output map */
    if ( print_flag->answer ) { /* print header */
	fprintf(stdout, "from_cat" );
	i = 0;
	while ( Upload[i].upload != END ) {
	    fprintf(stdout, "|%s", Upload[i].column );
	    i++;
	}
	fprintf(stdout, "\n" );
    } else if ( all && table_opt->answer ) { /* create new table */
	db_set_string (&stmt, "create table ");
	db_append_string (&stmt, table_opt->answer);
	db_append_string (&stmt, " (from_cat integer");
	
	j = 0;
	while ( Upload[j].upload != END ) {
	    db_append_string (&stmt, ", ");
		
	    switch ( Upload[j].upload ) {
		case CAT:
		    sprintf (buf2, "%s integer", Upload[j].column );
		    break;
		case DIST:
		case FROM_X:
		case FROM_Y:
		case TO_X:
		case TO_Y:
		case FROM_ALONG:
		case TO_ALONG:
		    sprintf (buf2, "%s double precision", Upload[j].column );
	    }
	    db_append_string (&stmt, buf2);
	    j++;
	}
	db_append_string (&stmt, " )");
	G_debug (3, "SQL: %s", db_get_string ( &stmt ));
	
        if ( db_execute_immediate (driver, &stmt) != DB_OK )
	    G_fatal_error ("Cannot create table: '%s'", db_get_string ( &stmt ) );

	if (db_grant_on_table (driver, table_opt->answer, DB_PRIV_SELECT, DB_GROUP|DB_PUBLIC ) != DB_OK )
	    G_fatal_error ( "Cannot grant privileges on table %s", table_opt->answer );
		
    } else { /* read existing cats from table */
	ncatexist = db_select_int( driver, Fi->table, Fi->key, NULL, &catexist);
        G_debug (1, "%d cats selected from the table", ncatexist );	
    }
    update_ok = update_err = update_exist = update_notexist = update_dupl = update_notfound;

    if ( !all ) count = nfcats;
    
    if ( driver ) 
        db_begin_transaction ( driver );

    for ( i = 0; i < count; i++ ) {
	/* Write line connecting nearest points */
	if ( Outp != NULL ) {
	    Vect_reset_line ( FPoints );
	    Vect_reset_cats ( FCats );

	    Vect_append_point ( FPoints, Near[i].from_x, Near[i].from_y, 0);
	    
	    if ( Near[i].dist == 0 ) { 
	        Vect_write_line ( Outp, GV_POINT, FPoints, FCats );
	    } else { 
	        Vect_append_point ( FPoints, Near[i].to_x, Near[i].to_y, 0);
	        Vect_write_line ( Outp, GV_LINE, FPoints, FCats );
	    }

	}

	if ( Near[i].count > 1 ) update_dupl++;
	if ( Near[i].count == 0 ) update_notfound++;
	
	if ( print_flag->answer || (all && !table_opt->answer) ) { /* print only */
	    fprintf(stdout, "%d", Near[i].from_cat );
	    j = 0;
	    while ( Upload[j].upload != END ) {
		if (  Near[i].count == 0 ) { /* no nearest found */
		    fprintf(stdout, "|null" );
		} else { 
		    switch ( Upload[j].upload ) {
			case CAT:
			    if ( Near[i].to_cat >= 0 ) 
			        fprintf(stdout, "|%d", Near[i].to_cat );
			    else 
			        fprintf(stdout, "|null" );

			    break;
			case DIST:
			    fprintf(stdout, "|%f", Near[i].dist );
			    break;
			case FROM_X:
			    fprintf(stdout, "|%f", Near[i].from_x );
			    break;
			case FROM_Y:
			    fprintf(stdout, "|%f", Near[i].from_y );
			    break;
			case TO_X:
			    fprintf(stdout, "|%f", Near[i].to_x );
			    break;
			case TO_Y:
			    fprintf(stdout, "|%f", Near[i].to_y );
			    break;
			case FROM_ALONG:
			    fprintf(stdout, "|%f", Near[i].from_along );
			    break;
			case TO_ALONG:
			    fprintf(stdout, "|%f", Near[i].to_along );
			    break;
		    }
		}
		j++;
	    }
	    fprintf(stdout, "\n" );
	} else if ( all )  { /* insert new record */
	    sprintf (buf1, "insert into %s values ( %d ", table_opt->answer, Near[i].from_cat);
	    db_set_string (&stmt, buf1);

	    j = 0;
	    while ( Upload[j].upload != END ) {
		db_append_string (&stmt, ",");
		    
		switch ( Upload[j].upload ) {
		    case CAT:
			sprintf (buf2, " %d", Near[i].to_cat );
			break;
		    case DIST:
			sprintf (buf2, " %f", Near[i].dist );
			break;
		    case FROM_X:
			sprintf (buf2, " %f", Near[i].from_x );
			break;
		    case FROM_Y:
			sprintf (buf2, " %f", Near[i].from_y );
			break;
		    case TO_X:
			sprintf (buf2, " %f", Near[i].to_x );
			break;
		    case TO_Y:
			sprintf (buf2, " %f", Near[i].to_y );
			break;
		    case FROM_ALONG:
			sprintf (buf2, " %f", Near[i].from_along );
			break;
		    case TO_ALONG:
			sprintf (buf2, " %f", Near[i].to_along );
			break;
		}
		db_append_string (&stmt, buf2);
		j++;
	    }
	    db_append_string (&stmt, " )");
	    G_debug ( 3, "SQL: %s", db_get_string ( &stmt ));
	    if ( db_execute_immediate (driver, &stmt) == DB_OK ){
		update_ok++;
	    } else {
		update_err++;
	    }
	} else { /* update table */
            /* check if exists in table */
	    cex = (int *) bsearch((void *) &(Near[i].from_cat), catexist, ncatexist, sizeof(int), cmp_exist);
	    if ( cex == NULL ){ /* cat does not exist in DB */
		update_notexist++;
		continue;
	    } 
	    update_exist++;
	    
	    sprintf (buf1, "update %s set", Fi->table);
	    db_set_string (&stmt, buf1);

	    j = 0;
	    while ( Upload[j].upload != END ) {
		if ( j > 0 ) db_append_string (&stmt, ",");
		    
	    	sprintf (buf2, " %s =", Upload[j].column );
		db_append_string (&stmt, buf2);
		
		if (  Near[i].count == 0 ) { /* no nearest found */
		    db_append_string (&stmt, " null");
		} else { 
		    switch ( Upload[j].upload ) {
			case CAT:
			    if ( Near[i].to_cat > 0 ) 
			        sprintf (buf2, " %d", Near[i].to_cat );
			    else 
			        sprintf (buf2, " null");

			    break;
			case DIST:
			    sprintf (buf2, " %f", Near[i].dist );
			    break;
			case FROM_X:
			    sprintf (buf2, " %f", Near[i].from_x );
			    break;
			case FROM_Y:
			    sprintf (buf2, " %f", Near[i].from_y );
			    break;
			case TO_X:
			    sprintf (buf2, " %f", Near[i].to_x );
			    break;
			case TO_Y:
			    sprintf (buf2, " %f", Near[i].to_y );
			    break;
			case FROM_ALONG:
			    sprintf (buf2, " %f", Near[i].from_along );
			    break;
			case TO_ALONG:
			    sprintf (buf2, " %f", Near[i].to_along );
			    break;
		    }
		    db_append_string (&stmt, buf2);
		}
		j++;
	    }
	    sprintf (buf2, " where %s = %d", Fi->key, Near[i].from_cat );
	    db_append_string (&stmt, buf2);
	    G_debug ( 2, "SQL: %s", db_get_string ( &stmt ));
	    if ( db_execute_immediate (driver, &stmt) == DB_OK ){
		update_ok++;
	    } else {
		update_err++;
	    }
	}

    }

    if ( driver ) 
        db_commit_transaction ( driver );
    
    /* print stats */
    fprintf (stderr,"Statistics:\n");
    fprintf (stderr,"%d categories with more than 1 feature in 'from'\n", update_dupl);
    fprintf (stderr,"%d categories - no nearest feature found\n", update_notfound);

    if ( !print_flag->answer ) { 
	db_close_database_shutdown_driver ( driver );
	db_free_string (&stmt);
	G_free ( catexist );

	/* print stats */
	if ( all ) {
	    fprintf (stderr,"%d distances calculated\n", count);
	    fprintf (stderr,"%d records inserted\n", update_ok);
	    fprintf (stderr,"%d insert errors\n", update_err);
	} else {
	    fprintf (stderr,"%d categories read from the map\n", nfcats);
	    fprintf (stderr,"%d categories exist in the table\n", ncatexist);
	    fprintf (stderr,"%d categories read from the map exist in the table\n", update_exist );
	    fprintf (stderr,"%d categories read from the map don't exist in the table\n", update_notexist);
	    fprintf (stderr,"%d records updated\n", update_ok);
	    fprintf (stderr,"%d update errors\n", update_err);
	}
    }
    
    Vect_close (&From);
    if ( Outp != NULL ) {
       fprintf (stderr,"\nBuilding topology for %s ...\n", Vect_get_name (Outp) );
       Vect_build ( Outp, stderr );	
       Vect_close ( Outp );
    }
    
    exit (0);
}

int cmp_near ( const void *pa, const void *pb) {
    NEAR *p1 = (NEAR *) pa;
    NEAR *p2 = (NEAR *) pb;

    if( p1->from_cat < p2->from_cat) return -1;
    if( p1->from_cat > p2->from_cat) return 1;
    return 0;
}

int cmp_exist ( const void *pa, const void *pb)
{
    int       *p1 = (int *) pa;
    int       *p2 = (int *) pb;

    if( *p1 < *p2 ) return -1;
    if( *p1 > *p2) return 1;
    return 0;
}
