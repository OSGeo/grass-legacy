/*
 * $Id$
 */

#include "gis.h" 
#include "raster.h"
#include "display.h" 
#include "dbmi.h"
#include "proto.h"

int main(int argc ,char **argv )
{
	struct Cell_head window ;
	char temp[128] ;
	int i, t, b, l, r, more, keyctype, coorctype[2], sqltype, cont; 
	int once=0, idx, button, d_x, d_y ;
	double   coor, dx, dy, dsq, tdsq, maxdsq, u_x, u_y ;
	char b_x[40], b_y[40];
		struct GModule *module;
        struct {
            struct Option *table, *key, *x, *y;
        } par;  
        typedef struct {
	    int  key;
	    double  coor[2];
        } LSITE;
	LSITE    *sites;
	int      alloc, nsites;
        char     buf[1024];
        dbString stmt;
        dbDriver *driver;
        dbHandle handle;
        dbCursor cursor;
        dbTable  *table;
        dbColumn *column;
        dbValue  *value;

	/* Initialize the GIS calls */
	G_gisinit (argv[0]) ;

	module = G_define_module();
	module->description =
		"View/edit attributes of selected DB site.";
						
        par.table               = G_define_option();
        par.table->key          = "table";
        par.table->type         = TYPE_STRING;
        par.table->required     = YES;
        par.table->description  = "table name"; 

        par.key               = G_define_option();
        par.key->key          = "key";
        par.key->type         = TYPE_STRING;
        par.key->required     = YES;
        par.key->description  = "key column name"; 					
	
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

	if (G_parser(argc, argv))
	    exit(-1);

	R_open_driver();

	if (D_get_cur_wind(temp))
		G_fatal_error("No current graphics window") ;

	if (D_set_cur_wind(temp))
		G_fatal_error("Current graphics window not available") ;

	/* Read in the map window associated with window */
	G_get_window(&window) ;

	if (D_check_map_window(&window))
		G_fatal_error("Setting graphics window") ;

	if (G_set_window(&window) == -1)
		G_fatal_error("Can't set current graphics window") ;

	/* Determine conversion factors */
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting graphics window coordinates") ;
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;
	
	/* read sites to list */
        db_init_string (&stmt);
        driver = db_start_driver(NULL);
        if (driver == NULL) exit(-1);
        db_init_handle (&handle);
        db_set_handle (&handle, NULL, NULL);
        if (db_open_database(driver, &handle) != DB_OK) exit(-1); 

        snprintf (buf,1023, "select %s, %s, %s from %s where %s > %f and %s < %f and %s > %f and %s < %f order by %s", par.key->answer,
		par.x->answer, par.y->answer, par.table->answer, par.x->answer, window.west, par.x->answer, window.east,
                par.y->answer, window.south, par.y->answer, window.north, par.key->answer );

        db_set_string (&stmt, buf);  
        fprintf (stderr, "%s\n", db_get_string(&stmt)); 
        if (db_open_select_cursor(driver, &stmt, &cursor, DB_SEQUENTIAL) != DB_OK)
            return (-1);
        table = db_get_cursor_table (&cursor);  	

	/* check if key is integer */
	column = db_get_table_column(table, 0);
        sqltype = db_get_column_sqltype (column);
        keyctype = db_sqltype_to_Ctype(sqltype);
        if ( keyctype != DB_C_TYPE_INT ) {
    	    fprintf ( stderr, "Column %s is not integer.\n", db_get_column_name ( column ) );
    	    exit (-1);
        }
	/* check if x,y are numbers */
	for (i=0; i<2; i++){
    	    column = db_get_table_column(table, i+1);
    	    sqltype = db_get_column_sqltype (column);
    	    coorctype[i] = db_sqltype_to_Ctype(sqltype);
    	    if ( coorctype[i] != DB_C_TYPE_INT && coorctype[i] != DB_C_TYPE_DOUBLE ) {
    		fprintf ( stderr, "Column %s is not number.\n", db_get_column_name ( column ) );
    		exit (-1);
    	    }
        }

	alloc = 1000;
	nsites = 0;
	sites = (LSITE *) G_malloc ( alloc * sizeof(LSITE));
        while(1) {
            if ( db_fetch (&cursor, DB_NEXT, &more ) != DB_OK ) return (-1);
            if (!more) break;
	    if ( nsites >= alloc ) {
		alloc += 1000;
		sites = (LSITE *) G_realloc ( sites, alloc * sizeof(LSITE));
	    }
            column = db_get_table_column(table, 0);
            value  = db_get_column_value(column);
            if ( db_test_value_isnull ( value ) ) continue;
	    sites[nsites].key = db_get_value_int(value);	    
	    
	    cont = 0;
	    for (i=0; i<2; i++){
    		column = db_get_table_column(table, i+1);
    		value  = db_get_column_value(column);
		if ( db_test_value_isnull ( value ) ) { cont = 1; break; }
		if ( coorctype[i] == DB_C_TYPE_INT ) {
            	    sites[nsites].coor[i] = db_get_value_int(value);
            	} 
		else if ( coorctype[i] == DB_C_TYPE_DOUBLE ){
            	    sites[nsites].coor[i] = db_get_value_double(value);
		}
	    }
	    if ( cont == 1 ) continue;	
	    nsites++;
        } 
        db_close_cursor(&cursor);	 																						    				
        fprintf ( stderr, "%d sites read from DB\n", nsites); 
	
	/* read selected point and find site */
	maxdsq = D_d_to_u_col( 100 ); /* maximum acceptable distance from site */
	do {
    	    show_buttons(once);
	    R_get_location_with_pointer(&d_x, &d_y, &button) ;
    	    if (!once) {
    		if (button == 2) continue;
        	if (button == 3) break;
    	    }
	    u_x = D_d_to_u_col( (double)d_x );
	    u_y = D_d_to_u_row( (double)d_y );
	    dsq = 2*maxdsq;   /* set initial value to more than maxdsq */
	    for ( i=0; i < nsites; i++) {
		dx = u_x - sites[i].coor[0];
		dy = u_y - sites[i].coor[1];
		tdsq = dx * dx  +  dy * dy;
		if ( tdsq < dsq ) {
		    dsq = tdsq;
		    idx = i;
		}	  
	    }
	    G_format_easting( u_x, b_x, G_projection());
            G_format_northing( u_y, b_y, G_projection());
	    if ( dsq < maxdsq ) {
                fprintf (stdout,"%s %s %s = %d\n", b_x, b_y, par.key->answer, sites[idx].key); 
		disp_attr( driver, par.table->answer, par.key->answer, sites[idx].key );
	    } else {
		fprintf (stdout,"%s %s nothing\n", b_x, b_y ); 
	    }
	} while (! once) ; 

        db_close_database(driver);
        db_shutdown_driver(driver);
        db_free_string (&stmt);  

	R_close_driver();
	exit(0);
}
