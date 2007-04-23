/******************************************************************************
 * MODULE:       v.in.db 
 *               
 * AUTHOR(S):    Radim Blazek
 *               
 * PURPOSE:      Create new vector from db table.
 * 	    
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 ******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <grass/gis.h>
#include <grass/dbmi.h>
#include <grass/Vect.h>
#include <grass/glocale.h>

int
main (int argc, char *argv[])
{
    int    i, cat, with_z, more, ctype, ret;
    char   buf[2000];
    int    count;
    double coor[3];
    int    ncoor;
    struct Option *driver_opt, *database_opt, *table_opt;
    struct Option *xcol_opt, *ycol_opt, *zcol_opt, *keycol_opt, *where_opt, *outvect;
    struct GModule *module;
    struct Map_info Map;
    struct line_pnts *Points;
    struct line_cats *Cats;
    dbString sql;
    dbDriver *driver;
    dbCursor cursor;
    dbTable *table;
    dbColumn *column;
    dbValue *value;
    struct field_info *fi;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("vector, import");
    module->description =
	_("Create new vector (points) from database table containing coordinates.");

    table_opt              = G_define_option();
    table_opt->key         = "table";
    table_opt->type        = TYPE_STRING;
    table_opt->required    = YES;
    table_opt->description = _("Input table name");

    driver_opt 		    = G_define_option();
    driver_opt->key 	    = "driver";
    driver_opt->type 	    = TYPE_STRING;
    driver_opt->options     = db_list_drivers();
    driver_opt->required    = NO;
    driver_opt->description = _("Input driver name");
    driver_opt->answer      = db_get_default_driver_name();

    database_opt 	      = G_define_option();
    database_opt->key 	      = "database";
    database_opt->type        = TYPE_STRING;
    database_opt->required    = NO;
    database_opt->description = _("Input database name");
    database_opt->answer      = db_get_default_database_name();

    xcol_opt 		  = G_define_option();
    xcol_opt->key 	  = "x";
    xcol_opt->type 	  = TYPE_STRING;
    xcol_opt->required    = YES;
    xcol_opt->description = _("x column name");

    ycol_opt 		  = G_define_option();
    ycol_opt->key 	  = "y";
    ycol_opt->type 	  = TYPE_STRING;
    ycol_opt->required    = YES;
    ycol_opt->description = _("y column name");

    zcol_opt 		  = G_define_option();
    zcol_opt->key 	  = "z";
    zcol_opt->type 	  = TYPE_STRING;
    zcol_opt->required    = NO;
    zcol_opt->description = _("z column name");

    keycol_opt 		    = G_define_option();
    keycol_opt->key 	    = "key";
    keycol_opt->type 	    = TYPE_STRING;
    keycol_opt->required    = YES;
    keycol_opt->description = _("category column name");

    where_opt = G_define_standard_option(G_OPT_WHERE);

    outvect = G_define_standard_option (G_OPT_V_OUTPUT);

    if (G_parser(argc, argv)) exit(EXIT_FAILURE);

    if ( zcol_opt->answer ) {
	with_z = 1; 
	ncoor = 3;
    } else {
        with_z = 0;
	ncoor = 2;
    }

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct ();
    db_init_string (&sql);

    if ( with_z ) Vect_open_new (&Map, outvect->answer, 1);
    else  Vect_open_new (&Map, outvect->answer, 0);

    Vect_hist_command ( &Map );

    fi = Vect_default_field_info ( &Map, 1, NULL, GV_1TABLE );

    /* Open driver */
    driver = db_start_driver_open_database ( driver_opt->answer, database_opt->answer );
    if ( driver == NULL ) {
	G_fatal_error ( _("Cannot open database <%s> by driver <%s>"), fi->database, fi->driver );
    }

    /* check if target table already exists */
    G_debug(3, "Output vector table <%s>, driver: <%s>, database: <%s>", 
                outvect->answer, db_get_default_driver_name(), db_get_default_database_name());
    if( db_table_exists ( db_get_default_driver_name(), 
        db_get_default_database_name(), outvect->answer) == 1 )
           G_fatal_error(_("Output vector table <%s> (driver: <%s>, database: <%s>) "
			   "already exists"),
           outvect->answer, db_get_default_driver_name(), db_get_default_database_name());

    /* Open select cursor */
    sprintf ( buf, "select %s, %s, %s", keycol_opt->answer, xcol_opt->answer, ycol_opt->answer );
    db_set_string ( &sql, buf);
  
    if ( with_z ) {
        sprintf ( buf, ", %s", zcol_opt->answer );
	db_append_string ( &sql,  buf );
    }

    sprintf ( buf, " from %s", table_opt->answer );
    db_append_string ( &sql, buf);
    
    if (where_opt->answer) {
       sprintf(buf, " WHERE %s", where_opt->answer);
       db_append_string ( &sql, buf );
    }
    G_debug ( 2, "SQL: %s", db_get_string(&sql) );

    if (db_open_select_cursor(driver, &sql, &cursor, DB_SEQUENTIAL) != DB_OK) {
        G_fatal_error ( _("Cannot open select cursor: '%s'"), db_get_string(&sql) );
    }

    table = db_get_cursor_table (&cursor);

    count = 0;
    while ( db_fetch (&cursor, DB_NEXT, &more ) == DB_OK && more ) {
	/* key column */
	column = db_get_table_column (table, 0);
        ctype = db_sqltype_to_Ctype( db_get_column_sqltype (column) );
	if ( ctype != DB_C_TYPE_INT ) G_fatal_error ( _("Key column must be integer") ); 
	value  = db_get_column_value(column);
	cat = db_get_value_int ( value );
	
	for ( i = 0; i < ncoor; i++ ) {
	    column = db_get_table_column (table, i + 1);
	    ctype = db_sqltype_to_Ctype( db_get_column_sqltype (column) );
	    if ( ctype != DB_C_TYPE_INT && ctype != DB_C_TYPE_DOUBLE ) 
		G_fatal_error ( _("x/y/z column must be integer or double") ); 
	    value  = db_get_column_value(column);
	    if ( ctype == DB_C_TYPE_INT )
	        coor[i] = (double) db_get_value_int ( value );
	    else 
                coor[i] = db_get_value_double ( value );
        }

    
	Vect_reset_line ( Points );
	Vect_reset_cats ( Cats );

        Vect_append_point ( Points, coor[0], coor[1], coor[2] );
	
        Vect_cat_set ( Cats, 1, cat );

        Vect_write_line (&Map, GV_POINT, Points, Cats);

        count++;
    }
    
    G_message ( _("%d points written to vector map"), count); 
    db_close_database_shutdown_driver ( driver );

    /* Copy table */
    if (where_opt -> answer)
	ret = db_copy_table_where (driver_opt->answer, database_opt->answer, table_opt->answer,
				   fi->driver, fi->database, fi->table,
				   where_opt -> answer);
    else
	ret = db_copy_table (driver_opt->answer, database_opt->answer, table_opt->answer,
			     fi->driver, fi->database, fi->table);

    if ( ret == DB_FAILED ) {
	G_warning ( _("Cannot copy table") );
    } else {
        Vect_map_add_dblink ( &Map, 1, NULL, fi->table, keycol_opt->answer, fi->database, fi->driver);
    }
    
    Vect_build (&Map, stdout);
    Vect_close (&Map);

    G_done_msg (""); 
    
    return (EXIT_SUCCESS);
}
