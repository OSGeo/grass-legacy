#include "gis.h"
#include "display.h"
#include "D.h"
#include "dbmi.h"
#include "raster.h"
#define   GLOBAL
#include "global.h"

int main (int argc, char **argv)
{
	char   msg[200];
	char   window_name[64] ;
	int    t, b, l, r, more, ctype[2], sqltype;
	int    i, icon, size, color, nplot=0, cont;	 
	struct Cell_head window ;
	struct GModule *module;
	struct {   
	    struct Option *table, *x, *y, *where, *icon, *size, *color;
	} par;  
	char     buf[1024];
        dbString stmt;
	dbDriver *driver;
	dbHandle handle;
	dbCursor cursor;
	dbTable  *table;
	dbColumn *column;
	dbValue  *value;   
	double   coor[2];

	module = G_define_module();
	module->description =
		"Displays sites from DB table in the active "
		"graphics monitor.";

        par.table               = G_define_option();
        par.table->key          = "table";
	par.table->type         = TYPE_STRING;
	par.table->required     = YES;
	par.table->description  = "table name";    

        par.x               = G_define_option();
        par.x->key          = "x";
	par.x->type         = TYPE_STRING;
	par.x->required     = YES;
	par.x->description  = "x column name";    	

        par.y               = G_define_option();
        par.y->key          = "y";
	par.y->type         = TYPE_STRING;
	par.y->required     = YES;
	par.y->description  = "y column name";    
	
        par.where               = G_define_option();
        par.where->key          = "where";
	par.where->type         = TYPE_STRING;
	par.where->required     = NO;
	par.where->description  = "where";    	

	par.color              = G_define_option() ;
	par.color->key         = "color" ;
	par.color->type        = TYPE_STRING ;
	par.color->required    = NO ;
	par.color->answer      = "gray" ;
	par.color->options     = D_color_list();
	par.color->description = "Color" ;

	par.size              = G_define_option() ;
	par.size->key         = "size" ;
	par.size->type        = TYPE_INTEGER ;
	par.size->required    = NO ;
	par.size->answer      = "5" ;
	par.size->options     = "0-1000" ;
	par.size->description = "Size, in pixels, in which the icon is to be drawn" ;

	par.icon              = G_define_option() ;
	par.icon->key         = "icon" ;
	par.icon->type        = TYPE_STRING ;
	par.icon->required    = NO ;
	par.icon->answer      = "x" ;
	par.icon->options     = "x,diamond,box,+" ;
	par.icon->description = "Specify the type of the icon" ;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check command line */

	if (G_parser(argc, argv)) exit(-1);

	color = D_translate_color(par.color->answer) ;
	if (color == 0)	{
		fprintf (stdout,"Don't know the color %s\n", par.color->answer);
		G_usage() ;
		exit(-1);
	}

	sscanf( par.size->answer,"%d", &size);

	if (! strcmp(par.icon->answer, "x"))
		icon = TYPE_X ;
	else if (! strcmp(par.icon->answer, "+"))
		icon = TYPE_PLUS ;
	else if (! strcmp(par.icon->answer, "box"))
		icon = TYPE_BOX ;
	else if (! strcmp(par.icon->answer, "diamond"))
		icon = TYPE_DIAMOND ;

	/* Setup driver and check important information */
	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current frame") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current frame not available") ;

	/* Read in the map window associated with window */
	G_get_window(&window) ;

	if (D_check_map_window(&window))
		G_fatal_error("Setting map region") ;

	if (G_set_window(&window) == -1)
		G_fatal_error("Current frame not settable") ;

	/* Determine conversion factors */
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen frame") ;
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;

	/* Do the plotting */
	R_standard_color (color) ;

	/* read DB */    
	db_init_string (&stmt);
	driver = db_start_driver(NULL);
	if (driver == NULL) exit(-1); 
	db_init_handle (&handle);
	db_set_handle (&handle, NULL, NULL);
	if (db_open_database(driver, &handle) != DB_OK) exit(-1); 

	snprintf (buf,1023, "select %s, %s from %s where %s > %f and %s < %f and %s > %f and %s < %f ", 
	          par.x->answer, par.y->answer, par.table->answer, par.x->answer, window.west, par.x->answer, window.east,
    		  par.y->answer, window.south, par.y->answer, window.north );
	db_set_string (&stmt, buf);
	if ( par.where->answer ) {
	    snprintf (buf,1023, " and %s", par.where->answer);	
	    db_append_string (&stmt, buf);
	}    
	fprintf (stderr, "%s\n", db_get_string(&stmt));
	if (db_open_select_cursor(driver, &stmt, &cursor, DB_SEQUENTIAL) != DB_OK)
    	    return (-1);
	table = db_get_cursor_table (&cursor); 
	
	for ( i=0; i<2; i++) {
	    column = db_get_table_column(table, i);
	    sqltype = db_get_column_sqltype (column);
	    ctype[i] = db_sqltype_to_Ctype(sqltype);
	    if ( ctype[i] != DB_C_TYPE_INT && ctype[i] != DB_C_TYPE_DOUBLE ) {
		fprintf ( stderr, "Column %s is not number.\n", db_get_column_name ( column ) );
		exit (-1);
	    }
	}

        while(1) {
	    if ( db_fetch (&cursor, DB_NEXT, &more ) != DB_OK ) return (-1);
	    if (!more) break;
	    cont = 0;
	    for ( i=0; i<2; i++) {    
		column = db_get_table_column(table, i);
		value  = db_get_column_value(column);
		if ( db_test_value_isnull ( value ) ) {
		    cont = 1;
		    break;	    
		}
		if ( ctype[i] == DB_C_TYPE_INT )
		    coor[i] = db_get_value_int(value);
		else if ( ctype[i] == DB_C_TYPE_DOUBLE )
		    coor[i] = db_get_value_double(value);
	    }
	    if ( cont == 1 ) continue;
	    draw_point ( coor[0], coor[1], icon, size );
	    nplot++;
	}

        db_close_cursor(&cursor); 
        db_close_database(driver);
        db_shutdown_driver(driver);
        db_free_string (&stmt); 

	D_add_to_list(G_recreate_command()) ;
	R_close_driver();
	
	fprintf (stderr, "%d sites plotted.\n", nplot); 
	exit(0);
}
