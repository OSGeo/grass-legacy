#include <string.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "local.h"

int vect_to_rast(char *vector_map,char *raster_map, int field, char *column, int nrows, 
	          int use, double value, int value_type )
{
    int i;
    char *vector_mapset;
    char msg[1024];
    struct Map_info Map;
    struct line_pnts *Points;
    int fd;	/* for raster map */
    int nareas, nlines;
    int stat;
    int format;
    int pass, npasses;
    long timer;

    /* Attributes */
    int nrec;
    int ctype;
    struct field_info *Fi;
    dbDriver *Driver;
    dbCatValArray cvarr;

    vector_mapset = G_find_vector2 (vector_map, "");
    if (vector_mapset == NULL) {
	G_fatal_error ( "Vector map <%s> not found", vector_map);
    }

    start_clock (&timer);
    start_clock (NULL);
    inform ("Loading vector information ...");
    Vect_set_open_level (2);
    Vect_open_old (&Map, vector_map, vector_mapset);

    if ( (use == USE_Z) && !(Vect_is_3d(&Map)) )
	G_fatal_error ("Vector map is not 3D");

    if ( use == USE_ATTR ) {
	db_CatValArray_init ( &cvarr );
	Fi = Vect_get_field( &Map, field);
	if ( Fi == NULL ) {
	    G_fatal_error ("Cannot get layer info for vector map");
	}

	Driver = db_start_driver_open_database ( Fi->driver, Fi->database );
	if (Driver == NULL)
	    G_fatal_error("Cannot open database %s by driver %s", Fi->database, Fi->driver);
	
	/* Note do not check if the column exists in the table because it may be expression */

	nrec = db_select_CatValArray ( Driver, Fi->table, Fi->key, column, NULL, &cvarr );
	G_debug (3, "nrec = %d", nrec );

	ctype = cvarr.ctype;
	if ( ctype != DB_C_TYPE_INT && ctype != DB_C_TYPE_DOUBLE )
	    G_fatal_error ( "Column type not supported" );

	if ( nrec < 0 ) G_fatal_error ("Cannot select data from table");
	sprintf (msg, "\n%d records selected from table", nrec);
	inform (msg);

	db_close_database_shutdown_driver(Driver);

	for ( i = 0; i < cvarr.n_values; i++ ) {
	    if ( ctype == DB_C_TYPE_INT ) {
		G_debug (3, "cat = %d val = %d", cvarr.value[i].cat, cvarr.value[i].val.i );
	    } else if ( ctype == DB_C_TYPE_DOUBLE ) {
		G_debug (3, "cat = %d val = %f", cvarr.value[i].cat, cvarr.value[i].val.d );
	    }
	}

	if ( ctype == DB_C_TYPE_INT ) format = USE_CELL;
	else if ( ctype == DB_C_TYPE_DOUBLE ) format = USE_DCELL;
	else G_fatal_error ("Column type not supported" );
    } else if ( use == USE_CAT ) {
	format = USE_CELL;
    } else if ( use == USE_VAL ) {
	format = value_type;
    } else if ( use == USE_Z ) {
	format = USE_DCELL;
    }

    Points = Vect_new_line_struct();
    inform(NULL);
    stop_clock(NULL);

    if ( use != USE_Z ) { 
	start_clock(NULL);
	inform ("Sorting areas by size ...");
	if((nareas = sort_areas (&Map, Points, field)) < 0) {
	    G_fatal_error ( "ERROR processing areas from vector map <%s>\n", vector_map);
	}
	sprintf (msg, " %d areas", nareas);
	inform (msg);
	inform (NULL);
	stop_clock(NULL);
    }

    if ( format == USE_CELL ) { 
        fd = G_open_cell_new (raster_map); 
    } else if ( format == USE_DCELL ) {
	fd = G_open_raster_new (raster_map, DCELL_TYPE);
    } else { G_fatal_error ("Unknown type"); }
    if (fd < 0) {
	G_fatal_error ( "Can't create raster map <%s>", raster_map);
    }

    nlines = 1;

    npasses = begin_rasterization(nrows, format);
    pass = 0;
    do {
	pass++;
	if (npasses > 1) fprintf (stdout,"Pass #%d (of %d)\n", pass, npasses);
	stat = 0;

	if ( (use != USE_Z) && nareas ) {
	    start_clock(NULL);
	    if (npasses > 1) inform ("  ");
	    inform ("Processing areas ...");

	    if(do_areas (&Map, Points, &cvarr, ctype, field, use, value, value_type) < 0) {
		fprintf (stderr, "\nERROR processing areas from vector map <%s>\n", vector_map);
		stat = -1;
		break;
	    }
	    sprintf (msg, " %d areas", nareas);
	    inform (msg);
	    inform (NULL);
	    stop_clock(NULL);
	}

	if (nlines) {
	    start_clock(NULL);
	    if (npasses > 1) inform ("  ");
	    inform ("Processing lines ...");

	    if((nlines = do_lines (&Map, Points, &cvarr, ctype, field, use, value, value_type)) < 0) {
		fprintf (stderr, "\nERROR processing lines from vector map <%s>\n", vector_map);
		stat = -1;
		break;
	    }
	    sprintf (msg, " %d lines", nlines);
	    inform (msg);
	    inform (NULL);
	    stop_clock (NULL);
	}

	start_clock (NULL);
	if(npasses > 1) inform ("  ");
	inform ("Writing raster map ...");

	stat = output_raster(fd);
	inform (NULL);
	stop_clock (NULL);
    } while (stat == 0);
    /* stat: 0 means repeat
     *       1 means done
     *      -1 means error
     */

    Vect_destroy_line_struct (Points);

    if (stat < 0) {
	G_unopen_cell(fd);
	return 1;
    }

    Vect_close ( &Map );

    start_clock(NULL);
    inform ("Creating support files for raster map ...");
    G_close_cell(fd);
    update_hist(raster_map, vector_map, vector_mapset, Map.head.orig_scale);
    update_colors (raster_map);
    update_cats(raster_map, vector_map, vector_mapset);
    inform(NULL);
    stop_clock(NULL);

    fprintf (stdout,"\n");
    sprintf (msg, "Raster map <%s> done.\nTotal processing time:", raster_map);
    inform(msg);
    inform(NULL);
    stop_clock (&timer);
    return 0;
}
