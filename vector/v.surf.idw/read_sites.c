#include <stdio.h>
#include <stdlib.h>
#include "glocale.h"
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "proto.h"

/* 1/2001 added field parameter MN
 * update 12/99 to read multi-dim sites properly MN
 * updated 28 June 1995 to use new sites API.
 * Only uses floating point attributes. 
 * mccauley
 */

void read_sites ( char *name, int field, char *col)
{
    extern long npoints;
    int   nrec, ctype, nlines, line;
    struct Map_info Map;
    struct field_info *Fi;
    dbDriver *Driver;
    dbCatValArray cvarr;
    struct line_pnts *Points;
    struct line_cats *Cats;
  
    Vect_set_open_level (2);
    Vect_open_old (&Map, name, "");

    db_CatValArray_init ( &cvarr );

    Fi = Vect_get_field( &Map, field);
    if ( Fi == NULL )
	G_fatal_error (_("Cannot read field info"));	

    Driver = db_start_driver_open_database ( Fi->driver, Fi->database );
    if ( Driver == NULL )
	G_fatal_error(_("Cannot open database %s by driver %s"), Fi->database, Fi->driver);

    nrec = db_select_CatValArray ( Driver, Fi->table, Fi->key, col, NULL, &cvarr );
    G_debug (3, "nrec = %d", nrec );

    ctype = cvarr.ctype;
    if ( ctype != DB_C_TYPE_INT && ctype != DB_C_TYPE_DOUBLE )
	G_fatal_error ( _("Column type not supported") );

    if ( nrec < 0 ) 
	G_fatal_error (_("Cannot select data from table"));

    G_message ( _("%d records selected from table"), nrec);

    db_close_database_shutdown_driver(Driver);
    

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct ();

    nlines = Vect_get_num_lines ( &Map );

    for ( line = 1; line <= nlines; line++) {
	int type, cat, ival, ret;
	double dval;
	
	type = Vect_read_line ( &Map, Points, Cats, line );

	if ( !(type & GV_POINTS ) ) continue;

	/* TODO: what to do with multiple cats */
	Vect_cat_get ( Cats, field, &cat );
	if ( cat < 0 ) continue;

	if ( ctype == DB_C_TYPE_INT ) {
	    ret = db_CatValArray_get_value_int ( &cvarr, cat, &ival );
	    dval = ival;
	} else { /* DB_C_TYPE_DOUBLE */
	    ret = db_CatValArray_get_value_double ( &cvarr, cat, &dval );
	}

	if ( ret != DB_OK ) {
	    G_warning (_("No record for line (cat = %d)"), cat );
	    continue;
	}
	    
	newpoint ( dval, Points->x[0], Points->y[0] );
    }

    db_CatValArray_free( &cvarr ) ;

    Vect_set_release_support ( &Map );
    Vect_close ( &Map );

    G_message ( _("%d points loaded\n"), npoints);
}

