#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <tcl.h>
#include <tk.h>
#include "gis.h"
#include "Vect.h"
#include "dbmi.h"
#include "raster.h"
#include "display.h"
#include "colors.h"
#include "form.h"
#include "global.h"
#include "proto.h"

static int last_cat_line;

int del_cat (int line, int field, int cat ) 
{
    int type, i;
    static struct line_pnts *Points = NULL;
    static struct line_cats *Cats = NULL;
    char buf[1000];
    
    G_debug (3, "del_cat() line = %d, field = %d, cat = %d", line, field, cat);

    if ( Points == NULL ) Points = Vect_new_line_struct ();
    if ( Cats == NULL ) Cats = Vect_new_cats_struct ();

    type = Vect_read_line(&Map, Points, Cats, line);
    Vect_field_cat_del ( Cats, field, cat);

    last_cat_line = Vect_rewrite_line (&Map, line, type, Points, Cats);
	
    check_record ( field, cat );

    Tcl_Eval ( Toolbox, "clear_cats" );	
    
    for (i = 0; i < Cats->n_cats; i++) {
	sprintf ( buf, "add_cat %d %d %d", last_cat_line, Cats->field[i], Cats->cat[i]);
	Tcl_Eval ( Toolbox, buf );
    }
    
    symb_updated_lines_set_from_map();
    symb_updated_nodes_set_from_map();
    G_debug (2, "  last_cat_line = %d", last_cat_line);
    
    return 0;
}

int add_cat (int field, int cat, int newrec ) 
{
    int type, i, ret;
    static struct line_pnts *Points = NULL;
    static struct line_cats *Cats = NULL;
    char buf[1000];
    
    G_debug (2, "add_cat() last_cat_line = %d, field = %d, cat = %d, newrec = %d", 
	                last_cat_line, field, cat, newrec);

    if ( Points == NULL ) Points = Vect_new_line_struct ();
    if ( Cats == NULL ) Cats = Vect_new_cats_struct ();

    type = Vect_read_line(&Map, Points, Cats, last_cat_line);
    Vect_cat_set ( Cats, field, cat );

    last_cat_line = Vect_rewrite_line (&Map, last_cat_line, type, Points, Cats);

    Tcl_Eval ( Toolbox, "clear_cats" );	
    
    for (i = 0; i < Cats->n_cats; i++) {
	sprintf ( buf, "add_cat %d %d %d", last_cat_line, Cats->field[i], Cats->cat[i]);
	Tcl_Eval ( Toolbox, buf );
    }

    if ( newrec ) {
	ret = new_record ( field, cat );
        if ( ret == 0 ) {
	    G_debug (2, "New record created.");
	} else if ( ret == 1 ) {
	    G_debug (2, "Record already existed.");
	} else if ( ret == -1 ) {
	    G_warning ("Cannot create new record.");
	}
    }
    
    symb_updated_lines_set_from_map();
    symb_updated_nodes_set_from_map();
    G_debug (2, "  last_cat_line = %d", last_cat_line);
    
    return 0;
}

/* 
 * Create new record in table 
 * returns: 0 created
 *          1 existed
 *         -1 error  
 */
int new_record ( int field, int cat ) 
{
    int ret, old;
    struct field_info *Fi;
    dbDriver *driver;
    dbValue value;
    dbString sql;
    char buf[1000];
    
    db_init_string (&sql);

    G_debug (2, "new_record() field = %d cat = %d", field, cat );
    
    Fi = Vect_get_field( &Map, field );
    if ( Fi == NULL ) { 
	i_message ( MSG_OK, MSGI_ERROR, "Database table for this field is not defined" );
	return -1;
    }

    /* Note: some drivers (dbf) writes date when db is closed so it is better open
     * and close database for each record, so that data may not be lost later */

    /* First check if already exists */
    driver = db_start_driver_open_database ( Fi->driver, Fi->database );
    if ( driver == NULL ) {
	sprintf (buf, "Cannot open database %s by driver %s", Fi->database, Fi->driver );
	i_message ( MSG_OK, MSGI_ERROR, buf );
	return -1;
    }
    ret = db_select_value ( driver, Fi->table, Fi->key, cat, Fi->key, &value );
    if ( ret == -1 ) {
	db_close_database_shutdown_driver ( driver );
	sprintf (buf, "Cannot select record from table %s", Fi->table );
	i_message ( MSG_OK, MSGI_ERROR, buf );
	return -1;
    }
    if ( ret == 0 ) { /* insert new record */
	sprintf ( buf, "insert into %s (%s) values (%d)", Fi->table, Fi->key, cat );
	db_set_string ( &sql, buf);
	G_debug ( 2, db_get_string ( &sql ) );
	ret = db_execute_immediate (driver, &sql);
	if ( ret != DB_OK ) {	
	    db_close_database_shutdown_driver ( driver );
	    sprintf (buf, "Cannot insert new record: %s", db_get_string(&sql) );
	    i_message ( MSG_OK, MSGI_ERROR, buf );
	    return -1;
	}
	old = 0;
    } else { /* record already existed */
	old = 1;
    }
    
    db_close_database_shutdown_driver ( driver );

    return old;
}


/* Display categories */
int display_cats (void)
{
    int j, sxn, syn, line, type;
    int button;
    double x, y, thresh;
    struct line_pnts *Points;
    struct line_cats *Cats;
    char buf[1000];
    dbString cmd; 
    
    G_debug (2, "display_cats()");

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    db_init_string (&cmd);
    
    i_prompt ( "Display categories:"); 
    i_prompt_buttons ( "Select line", "", "Quit tool"); 
    
    driver_open();
    
    /* TODO: use some better threshold */
    thresh = fabs ( D_d_to_u_col ( 10 ) - D_d_to_u_col ( 0 ) ) ; 
    G_debug (2, "thresh = %f", thresh );
    
    F_clear ();
    line = 0;
    last_cat_line = 0;
    sxn = COOR_NULL; syn = COOR_NULL;
    while ( 1 ) {
	/* Get next coordinate */
        R_set_update_function ( update );
        R_get_location_with_pointer ( &sxn, &syn, &button); 
	    
	x =  D_d_to_u_col ( sxn );
	y =  D_d_to_u_row ( syn );
	G_debug (2, "button = %d x = %d = %f y = %d = %f", button, sxn, x, syn, y);

	/* Display last highlighted in normal color */
        G_debug (2, "  last_cat_line = %d", last_cat_line);
	if ( last_cat_line > 0 ) {
	    display_line ( last_cat_line, SYMB_DEFAULT, 1);
	}

	if ( button == 0 || button == 3 ) break; /* Quit tool */

	if ( button == 1 ) { /* Confirm / select */
	    F_clear ();
	    /* Find nearest point or line (points first!) */
	    line = Vect_find_line (&Map, x, y, 0, GV_POINTS, thresh, 0, 0);
	    G_debug (2, "point found = %d", line );
	    if ( line == 0 ) line = Vect_find_line (&Map, x, y, 0, GV_LINE|GV_BOUNDARY, thresh, 0, 0);
	    G_debug (2, "line found = %d", line );
	    
	    /* Display new selected line if any */
	    if ( line > 0 ) {
		display_line ( line, SYMB_HIGHLIGHT, 1);
		type = Vect_read_line(&Map, Points, Cats, line);

		Tcl_Eval ( Toolbox, "mk_cats" ); /* mk_cats checks if already opened */
	        Tcl_Eval ( Toolbox, "clear_cats" );	
		    
		for (j = 0; j < Cats->n_cats; j++) {
		    G_debug(3, "field = %d category = %d", Cats->field[j], Cats->cat[j]);

		    sprintf ( buf, "add_cat %d %d %d", line, Cats->field[j], Cats->cat[j]);
		    Tcl_Eval ( Toolbox, buf );

		}
	    }
	    last_cat_line = line;
	}
    }
    Tcl_Eval ( Toolbox, "destroy_cats" );
		
    driver_close();
    
    i_prompt (""); 
    i_prompt_buttons ( "", "", ""); 
    i_coor ( COOR_NULL, COOR_NULL); 
    
    G_debug (3, "display_cats(): End");

    return 1;
}
/* Display attributes */
int display_attributes (void)
{
    int j, sxn, syn, line, last_line, type;
    int button;
    static int first_form = 1;
    double x, y, thresh;
    struct line_pnts *Points;
    struct line_cats *Cats;
    char buf[1000], title[500];
    char *form;
    dbString html; 
    struct field_info *Fi;
    
    G_debug (2, "display_attributes()");

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    db_init_string (&html);
    
    i_prompt ( "Display attributes:"); 
    i_prompt_buttons ( "Select line", "", "Quit tool"); 
    
    driver_open();
    
    /* TODO: use some better threshold */
    thresh = fabs ( D_d_to_u_col ( 10 ) - D_d_to_u_col ( 0 ) ) ; 
    G_debug (2, "thresh = %f", thresh );
    
    F_clear ();
    line = 0;
    last_line = 0;
    sxn = COOR_NULL; syn = COOR_NULL;
    while ( 1 ) {
	/* Get next coordinate */
        R_set_update_function ( update );
        R_get_location_with_pointer ( &sxn, &syn, &button); 
	    
	x =  D_d_to_u_col ( sxn );
	y =  D_d_to_u_row ( syn );
	G_debug (3, "button = %d x = %d = %f y = %d = %f", button, sxn, x, syn, y);

	/* Display last highlighted in normal color */
	if ( last_line > 0 ) {
	    display_line ( last_line, SYMB_DEFAULT, 1);
	}

	if ( button == 0 || button == 3 ) break; /* Quit tool */

	if ( button == 1 ) { /* Confirm / select */
	    F_clear ();
	    /* Find nearest point or line (points first!) */
	    line = Vect_find_line (&Map, x, y, 0, GV_POINTS, thresh, 0, 0);
	    G_debug (2, "point found = %d", line );
	    if ( line == 0 ) line = Vect_find_line (&Map, x, y, 0, GV_LINE|GV_BOUNDARY, thresh, 0, 0);
	    G_debug (2, "line found = %d", line );
	    
	    /* Display new selected line if any */
	    if ( line > 0 ) {
		display_line ( line, SYMB_HIGHLIGHT, 1);
		type = Vect_read_line(&Map, Points, Cats, line);

		/* Note: F_open() must be run first time with closed monitor, otherwise next
		 *         *        attempt to open driver hangs until form child process is killed */
		if ( first_form ) { 
		     driver_close();
		     F_open ( "", "" );
	             F_clear ();
		     driver_open(); 
		     first_form = 0; 
		}

		if ( Cats->n_cats > 0 ) {
		    for (j = 0; j < Cats->n_cats; j++) {
			G_debug(3, "field = %d category = %d", Cats->field[j], Cats->cat[j]);

			sprintf (title, "Field %d", Cats->field[j] );
			db_set_string (&html, ""); 
			db_append_string (&html, "<HTML><HEAD><TITLE>Attributes</TITLE><BODY>"); 

			sprintf(buf, "field: %d<BR>category: %d<BR>", Cats->field[j], Cats->cat[j] );
			db_append_string (&html, buf);

			Fi = Vect_get_field( &Map, Cats->field[j]);
			if (Fi == NULL) {
			    db_append_string (&html, "Database connection not defined<BR>" );
			} else {
			    sprintf(buf, "driver: %s<BR>database: %s<BR>table: %s<BR>key column: %s<BR>",
					 Fi->driver, Fi->database, Fi->table, Fi->key);
			    db_append_string (&html, buf);
			    
			    F_generate ( Fi->driver, Fi->database, Fi->table, Fi->key, Cats->cat[j], 
				     NULL, NULL, F_EDIT, F_HTML, &form);
			    
			    db_append_string (&html, form); 
			    G_free (form);
			    G_free(Fi);
			}
			db_append_string (&html, "</BODY></HTML>"); 
			G_debug ( 3, db_get_string (&html) ); 
			F_open ( title, db_get_string(&html) );
		    }
		} else {
		    sprintf (title, "Line %d", line );
		    db_set_string (&html, ""); 
		    db_append_string (&html, "<HTML><HEAD><TITLE>Attributes</TITLE><BODY>"); 
		    db_append_string (&html, "No categories"); 
		    db_append_string (&html, "</BODY></HTML>"); 
		    G_debug ( 3, db_get_string (&html) ); 
		    F_open ( title, db_get_string(&html) );
		}
	    }
	    last_line = line;
	}
    }
    F_clear ();
    F_close ();

    driver_close();
    
    i_prompt (""); 
    i_prompt_buttons ( "", "", ""); 
    i_coor ( COOR_NULL, COOR_NULL); 
    
    G_debug (3, "display_attributes(): End");

    return 1;
}

/* 
 * Check if deleted category exists in category index, ask user if not and delete it if requested
 * 
 * returns: 
 */
int check_record ( int field, int cat ) 
{
    int ret, field_index, type, id;
    struct field_info *Fi;
    dbDriver *driver;
    dbValue value;
    dbString sql;
    char buf[1000];
    
    db_init_string (&sql);

    G_debug (3, "check_record() field = %d cat = %d", field, cat );

    Fi = Vect_get_field( &Map, field );
    if ( Fi == NULL ) {  /* no table */
	return 0;
    }

    /* Are there still elemets with this category */
    field_index = Vect_cidx_get_field_index ( &Map, field );
    G_debug (3, "field_index = %d", field_index );
    if ( field_index >= 0 ) {
        ret = Vect_cidx_find_next ( &Map, field_index, cat, GV_POINTS|GV_LINES, 0, &type, &id );
	G_debug (3, "ret = %d", ret );

	if ( ret >= 0 ) return 0; /* Category exists in map */
    }

    /* Does record exist ? */
    driver = db_start_driver_open_database ( Fi->driver, Fi->database );
    if ( driver == NULL ) {
	sprintf (buf, "Cannot open database %s by driver %s", Fi->database, Fi->driver );
	i_message ( MSG_OK, MSGI_ERROR, buf );
	return -1;
    }
    ret = db_select_value ( driver, Fi->table, Fi->key, cat, Fi->key, &value );
    G_debug (3, "n records = %d", ret );
    if ( ret == -1 ) {
	db_close_database_shutdown_driver ( driver );
	sprintf (buf, "Cannot select record from table %s", Fi->table );
	i_message ( MSG_OK, MSGI_ERROR, buf );
	return -1;
    }
    
    if ( ret == 0 ) return 0;

    sprintf (buf, "There are no more features with category %d (field %d) in the map, but there is "
	          "record in the table. Delete this record?", cat, field );
    ret = i_message ( MSG_YESNO, MSGI_QUESTION, buf );
    
    if ( ret == 1 ) return 0;  /* No, do not delete */

    sprintf ( buf, "delete from %s where %s = %d", Fi->table, Fi->key, cat );
    db_set_string ( &sql, buf);
    G_debug ( 2, db_get_string ( &sql ) );
    ret = db_execute_immediate (driver, &sql);
    if ( ret != DB_OK ) {	
	db_close_database_shutdown_driver ( driver );
	sprintf (buf, "Cannot delete record: %s", db_get_string(&sql) );
	i_message ( MSG_OK, MSGI_ERROR, buf );
	return -1;
    }
    
    db_close_database_shutdown_driver ( driver );

    return 0;
}

