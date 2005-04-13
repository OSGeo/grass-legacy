/***************************************************************
 *
 * MODULE:       v.db.select
 * 
 * AUTHOR(S):    Radim Blazek
 *               
 * PURPOSE:      Print vector attributes
 *               
 * COPYRIGHT:    (C) 2005 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 **************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "glocale.h"
#include "gis.h"
#include "Vect.h"
#include "dbmi.h" 

int main (int argc, char **argv)
{
    struct GModule *module;
    struct Option *map_opt, *field_opt, *fs_opt, *vs_opt, *nv_opt, *col_opt;
    struct Flag *c_flag, *v_flag;
    dbDriver *driver;
    dbString sql, value_string;
    dbCursor cursor;
    dbTable *table;
    dbColumn *column;
    dbValue *value;
    struct field_info *Fi;
    int field, ncols, col, more;
    struct Map_info Map;
    char query[1024];
    
    module = G_define_module();
    module->description = "Print vector attributes";

    map_opt = G_define_standard_option(G_OPT_V_MAP);
    field_opt = G_define_standard_option(G_OPT_V_FIELD) ;

    col_opt 		= G_define_option();
    col_opt->key 	= "column";
    col_opt->type 	= TYPE_STRING;
    col_opt->required 	= NO;
    col_opt->description = _("single attribute column");

    fs_opt 		= G_define_option();
    fs_opt->key 	= "fs";
    fs_opt->type 	= TYPE_STRING;
    fs_opt->required 	= NO;
    fs_opt->description = _("output field separator");
    fs_opt->answer	= "|";

    vs_opt 		= G_define_option();
    vs_opt->key 	= "vs";
    vs_opt->type 	= TYPE_STRING;
    vs_opt->required 	= NO;
    vs_opt->description = _("output vertical record separator");

    nv_opt 		= G_define_option();
    nv_opt->key 	= "nv";
    nv_opt->type 	= TYPE_STRING;
    nv_opt->required 	= NO;
    nv_opt->description = _("null value indicator");

    c_flag		= G_define_flag();
    c_flag->key		= 'c';
    c_flag->description	= _("do not include column names in output");

    v_flag		= G_define_flag();
    v_flag->key		= 'v';
    v_flag->description	= _("vertical output (instead of horizontal)");

    G_gisinit (argv[0]);

    if (G_parser (argc, argv))
        exit (-1);

    /* set input vector file name and mapset */
    field = atoi (field_opt->answer);

    db_init_string (&sql);
    db_init_string (&value_string);

    Vect_open_old_head ( &Map, map_opt->answer, "");

    if ( (Fi = Vect_get_field ( &Map, field)) == NULL ) 
	G_fatal_error(_("Database connection not defined"));

    driver = db_start_driver_open_database ( Fi->driver, Fi->database );

    if ( col_opt->answer )
      sprintf(query, "SELECT %s FROM ", col_opt->answer);
    else
      sprintf(query, "SELECT * FROM ");

    db_set_string ( &sql, query );
    db_append_string ( &sql, Fi->table );

    if (db_open_select_cursor(driver, &sql, &cursor, DB_SEQUENTIAL) != DB_OK)
	G_fatal_error(_("Cannot open select cursor"));

    table = db_get_cursor_table (&cursor);
    ncols = db_get_table_number_of_columns (table);

    /* column names if horizontal output */
    if ( !v_flag->answer && !c_flag->answer ) {
	for (col = 0; col < ncols; col++) {
	    column = db_get_table_column(table, col);
	    if (col) fprintf (stdout, "%s", fs_opt->answer);
	    fprintf (stdout, "%s", db_get_column_name (column));
	}
	fprintf (stdout, "\n");
    }

    /* fetch the data */
    while(1) {
	if(db_fetch (&cursor, DB_NEXT, &more) != DB_OK)
	    G_fatal_error(_("Cannot fetch data"));

	if (!more) break;

	for (col = 0; col < ncols; col++) {
	    column = db_get_table_column(table, col);
	    value  = db_get_column_value(column);
	    db_convert_column_value_to_string (column, &value_string);

	    if ( !c_flag->answer && v_flag->answer ) 
		fprintf (stdout, "%s%s", db_get_column_name (column), fs_opt->answer );

	    if (col && !v_flag->answer )
		fprintf (stdout, "%s", fs_opt->answer );

	    if ( nv_opt->answer && db_test_value_isnull(value) )
	        fprintf (stdout, "%s", nv_opt->answer);	
	    else
		fprintf (stdout, "%s", db_get_string (&value_string));

	    if ( v_flag->answer )
		fprintf (stdout, "\n");
	}
	if ( !v_flag->answer )
	    fprintf (stdout, "\n");
    	else if ( vs_opt->answer )
	    fprintf (stdout, "%s\n", vs_opt->answer);
    }

    if ( !driver )
        G_fatal_error(_("Cannot open database %s by driver %s"), Fi->database, Fi->driver);

    db_close_database_shutdown_driver(driver);
    Vect_close ( &Map);
    
    exit(0);
}
