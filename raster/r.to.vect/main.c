#define MAIN

#include <stdio.h> 
#include <stdlib.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "global.h"
#include "lines.h"

/* 
* Attributes for lines are ignored. For points and area by default unique new category is assigned
* to each and raster value is written to 'value' column. Labels are written to 'label' column
* if exists. If value flag (-v) is used and type is CELL, raster values are used as categories. 
*/

int main (int argc, char *argv[])
{
    struct GModule *module;
    struct Option *in_opt, *out_opt, *feature_opt;
    struct Flag *smooth_flg, *value_flg;  
    int feature;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description = "Converts a raster map into a vector map layer.";

    in_opt = G_define_option();
    in_opt->key             = "input";
    in_opt->type            = TYPE_STRING;
    in_opt->required        = YES;
    in_opt->multiple        = NO;
    in_opt->gisprompt       = "old,cell,raster";
    in_opt->description     = "raster input file";

    out_opt = G_define_standard_option(G_OPT_V_OUTPUT);

    feature_opt = G_define_option();
    feature_opt->key            = "feature";
    feature_opt->type           = TYPE_STRING;
    feature_opt->required       = YES;
    feature_opt->multiple       = NO;
    feature_opt->options        = "point,line,area";
    feature_opt->answer         = "line";
    feature_opt->description    = "Feature type";

    smooth_flg = G_define_flag();
    smooth_flg->key = 's';
    smooth_flg->description = "Smooth Corners";

    value_flg = G_define_flag();
    value_flg->key = 'v';
    value_flg->description = "Use raster values as categories instead of unique sequence (CELL only)";

    if (G_parser (argc, argv)) exit (-1);

    cell_name = G_store ( in_opt->answer );
    feature = Vect_option_to_types ( feature_opt );
    smooth_flag = (smooth_flg->answer) ? SMOOTH : NO_SMOOTH;
    value_flag = value_flg->answer;

    /* Open files */
    if ( (mapset = G_find_cell(in_opt->answer,"")) == NULL )
	G_fatal_error ( "Raster '%s' not found", in_opt->answer);

    if ( (input_fd = G_open_cell_old(in_opt->answer,mapset)) < 0 )
	G_fatal_error ( "Could not open raster '%s'", in_opt->answer);

    data_type = G_raster_map_type(in_opt->answer,mapset);
    data_size = G_raster_size(data_type);
    G_get_window(&cell_head);

    if ( value_flag && data_type != CELL_TYPE ) {
	G_warning ( "Raster is not CELL, '-v' flag ignored, raster values will be written to the table.");
	value_flag = 0;
    }
    
    Vect_open_new (&Map, out_opt->answer, 0);

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();

    /* Open category labels */
    if ( data_type == CELL_TYPE && (G_read_cats(cell_name, mapset, &RastCats) == 0) )
	has_cats = 1;
    else
	has_cats = 0;

    /* Create table */
    if ( (feature & (GV_AREA | GV_POINT)) && (!value_flag || (value_flag && has_cats)) ) {
	char buf[1000];

	db_init_string (&sql);
	db_init_string (&label);

	Fi = Vect_default_field_info ( &Map, 1, NULL, GV_1TABLE );
	Vect_map_add_dblink ( &Map, 1, NULL, Fi->table, "cat", Fi->database, Fi->driver);

        driver = db_start_driver_open_database ( Fi->driver, Fi->database );
	if ( driver == NULL ) 
	    G_fatal_error ( "Cannot open database %s by driver %s", Fi->database, Fi->driver );
	    
	/* Create new table */
	db_zero_string (&sql);
	sprintf ( buf, "create table %s ( cat integer", Fi->table );
	db_append_string ( &sql, buf );
	
	if ( !value_flag ) { /* add value to the table */
	    if ( data_type == CELL_TYPE )
	        db_append_string ( &sql, ", value integer" );
	    else 
	        db_append_string ( &sql, ", value double precision" );
	}

	if ( has_cats ) {
	    int i, len, clen;

	    /* Get maximum column length */
	    clen = 0;
	    for ( i = 0; i < RastCats.ncats; i++) {
		len = strlen ( RastCats.labels[i] );
		if ( len > clen ) clen = len;
	    }
	    clen += 10; 

	    sprintf ( buf, ", label varchar(%d)", clen );
	    db_append_string ( &sql, buf );
	}
	
	db_append_string ( &sql, ")" );

	G_debug ( 3, db_get_string ( &sql ) );

	if (db_execute_immediate (driver, &sql) != DB_OK )
	    G_fatal_error ( "Cannot create table: %s", db_get_string ( &sql )  );

	if ( db_create_index2(driver, Fi->table, "cat" ) != DB_OK )
	    G_warning ( "Cannot create index" );

	if (db_grant_on_table (driver, Fi->table, DB_PRIV_SELECT, DB_GROUP|DB_PUBLIC ) != DB_OK )
	    G_fatal_error ( "Cannot grant privileges on table %s", Fi->table );

	db_begin_transaction ( driver );

	if ( value_flag ) { /* we can write out category labels here */
	    int i, cat;
	    
	    for ( i = 0; i < RastCats.ncats; i++) {
		cat = (int) RastCats.q.table[i].dLow; /* cats are in dLow/High not in cLow/High !!! */ 
		G_debug ( 3, "%d cat = %d label = %s", i, cat, RastCats.labels[i] );
		
		db_set_string ( &label, RastCats.labels[i]);
		db_double_quote_string ( &label );
		sprintf (buf, "insert into %s values ( %d, '%s')", Fi->table, cat, db_get_string(&label) );
		db_set_string ( &sql, buf);
		G_debug ( 3, db_get_string ( &sql ) );

		if (db_execute_immediate (driver, &sql) != DB_OK ) 
		    G_fatal_error ( "Cannot insert into table: %s", db_get_string ( &sql )  );
	    }
	}
    } else {
	driver = NULL;
    }

    /* init variables for lines and areas */
    first_read = 1;
    last_read = 0;
    direction = FORWARD;
    row_length = cell_head.cols;
    n_rows = cell_head.rows;
    row_count = 0;

    if ( feature == GV_LINE ) {
        alloc_lines_bufs(row_length + 2);
	extract_lines();
    } else if ( feature == GV_AREA ) {
        alloc_areas_bufs(row_length + 2);
	extract_areas();
    } else { /* GV_POINT */
	extract_points ();
    }

    G_close_cell(input_fd);

    if ( has_cats )
	G_free_cats(&RastCats);
    
    if ( driver != NULL ) {
	db_commit_transaction ( driver );
	db_close_database_shutdown_driver ( driver );
    }

    Vect_build (&Map, stderr);
    Vect_close (&Map);

    exit(0);
}
