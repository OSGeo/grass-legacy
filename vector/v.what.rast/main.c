/* ***************************************************************
 * *
 * * MODULE:       v.what.rast
 * * 
 * * AUTHOR(S):    Radim Blazek (using r.what)
 * *               Michael Shapiro, U.S. Army Construction Engineering Research Laboratory (r.what)
 * *               
 * * PURPOSE:      Query raster map
 * *               
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * **************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "glocale.h"

struct order {
    int cat;      /* point category */
    int count;    /* nuber of points with category 'cat' */
    int row;      
    int col;
    CELL value;
    DCELL dvalue;  /* used for FCELL and DCELL */
};

static int by_row(const void *, const void *);
static int by_cat(const void *, const void *);
static int srch_cat( const void *, const void * ); 


int main(int argc,char *argv[])
{
    char *mapset;
    int i, j, nlines, type, field, cat;
    int fd;
    /* struct Categories RCats; */  /* TODO */ 
    struct Cell_head window;
    RASTER_MAP_TYPE out_type;
    CELL *cell;
    DCELL *dcell;
    double drow, dcol;
    char buf[2000];
    struct Option *vect_opt, *rast_opt, *field_opt, *col_opt;
    int Cache_size;
    struct order *cache;
    int cur_row;
    struct GModule *module;

    struct Map_info Map; 
    struct line_pnts *Points;
    struct line_cats *Cats;
    int    point;
    int    point_cnt;       /* number of points in cache */ 
    int    outside_cnt; /* points outside region */
    int    nocat_cnt;   /* points inside region but without category */
    int    dupl_cnt;   /* duplicate categories */
    BOUND_BOX box;

    int    *catexst, *cex;
    struct field_info *Fi;
    dbString stmt; 
    dbDriver *driver;
    int   select, norec_cnt, update_cnt, upderr_cnt, col_type;

    G_gisinit (argv[0]);

    module              = G_define_module();
    module->description = "Upload raster values at positions of vector points to the table.";

    vect_opt = G_define_standard_option(G_OPT_V_INPUT);
    vect_opt->key        = "vect" ;
    vect_opt->description= "Name of input vector points map";

    rast_opt = G_define_option() ;
    rast_opt->key        = "rast" ;
    rast_opt->type       = TYPE_STRING ;
    rast_opt->required   = YES ;
    rast_opt->gisprompt  = "old,cell,raster" ;
    rast_opt->description= "Name of existing raster map" ;

    field_opt = G_define_standard_option(G_OPT_V_FIELD);

    col_opt = G_define_option() ;
    col_opt->key        = "col" ;
    col_opt->type       = TYPE_STRING ;
    col_opt->required   = YES ;
    col_opt->description= "Column name (will be updated by raster values)" ;

    if (G_parser(argc, argv))
      exit(-1);

    field = atoi( field_opt->answer );

    db_init_string (&stmt);	
    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    G_get_window (&window);
    Vect_region_box ( &window, &box ); /* T and B set to +/- PORT_DOUBLE_MAX */

    /* Open vector */
    if ( (mapset = G_find_vector2 (vect_opt->answer, "")) == NULL) 
	G_fatal_error ( "Cannot find vector map");

    Vect_set_open_level (2); 
    Vect_open_old (&Map, vect_opt->answer, mapset);

    Fi = Vect_get_field ( &Map, field);
    if ( Fi == NULL ) G_fatal_error ( "Cannot get field info for vector");
    
    /* Open driver */
    driver = db_start_driver_open_database ( Fi->driver, Fi->database );
    if ( driver == NULL ) {
        G_fatal_error ( "Cannot open database %s by driver %s", Fi->database, Fi->driver );
    }
    
    /* Open raster */
    if ( (mapset = G_find_cell2 ( rast_opt->answer, "") ) == NULL )
	G_fatal_error ( "Cannot find raster map");
    
    if( (fd = G_open_cell_old (rast_opt->answer, mapset)) < 0 )
	G_fatal_error ( "Cannot open raster map");

    out_type = G_raster_map_type(rast_opt->answer, mapset);

    /* TODO: Later possibly category labels */
    /* 
    if ( G_read_cats (name, mapset, &RCats) < 0 )
    	G_fatal_error ( "Cannot read category file");
    */

    /* Check column type */
    col_type = db_column_Ctype ( driver, Fi->table, col_opt->answer );

    if ( col_type == -1 ) G_fatal_error ( "Column not found" );

    if ( col_type != DB_C_TYPE_INT && col_type != DB_C_TYPE_DOUBLE )
	 G_fatal_error ( "Column type is not supported" ); 

    if ( out_type == CELL_TYPE && col_type == DB_C_TYPE_DOUBLE ) 
	G_warning ( "Raster type is integer and column type is float" );
    
    if ( out_type != CELL_TYPE && col_type == DB_C_TYPE_INT ) 
	G_warning ( "Raster type is float and column type is integer, some data lost!!!!" );
	     
    /* Read vector points to cache */
    Cache_size = Vect_get_num_primitives ( &Map, GV_POINT ); 
        /* Note: Some space may be wasted (outside region or no category) */

    cache = (struct order *) G_calloc ( Cache_size, sizeof (struct order) );

    point_cnt = outside_cnt = nocat_cnt = 0;

    nlines = Vect_get_num_lines ( &Map );

    for ( i = 1; i <= nlines; i++ ) {
        type = Vect_read_line (&Map, Points, Cats, i);
	G_debug ( 4, "line = %d type = %d", i, type );

        /* check type */
	if ( !(type & GV_POINT) ) continue; /* Points only */

	/* check region */
	if ( ! Vect_point_in_box ( Points->x[0], Points->y[0], 0.0, &box) ) {
	    outside_cnt++;
	    continue;
	}

	Vect_cat_get (Cats, field, &cat);
	if ( cat < 0 ) {  /* no category of given field */
	    nocat_cnt++;
	    continue;
	}

	G_debug ( 4, "    cat = %d", cat );

	/* Add point to cache */
	drow = G_northing_to_row (Points->y[0], &window);
	dcol = G_easting_to_col (Points->x[0], &window);

	/* a special case.
	*   if north falls at southern edge, or east falls on eastern edge,
	*   the point will appear outside the window.
	*   So, for these edges, bring the point inside the window
	*/
	if (drow == window.rows) drow--;
	if (dcol == window.cols) dcol--;

	cache[point_cnt].row = (int) drow;
        cache[point_cnt].col = (int) dcol;
	cache[point_cnt].cat = cat;
	cache[point_cnt].count = 1;
	point_cnt++;
    }

    Vect_set_db_updated ( &Map );
    Vect_close ( &Map );

    /* Cache may contain duplicate categories, sort by cat, find and remove duplicates 
     * and recalc count and decrease point_cnt  */
    qsort (cache, point_cnt, sizeof (struct order), by_cat);

    i = 1;
    while ( i < point_cnt ) {
        if ( cache[i].cat == cache[i-1].cat ) {
	    cache[i-1].count++;
	    for ( j = i; j < point_cnt - 1; j++ ) {
		cache[j].row = cache[j+1].row; 
		cache[j].col = cache[j+1].col; 
		cache[j].cat = cache[j+1].cat; 
		cache[j].count = cache[j+1].count; 
	    }
	    point_cnt--;
	    continue;
	}
        i++;
    }

    /* Report number of points not used */
    if ( outside_cnt )
	G_warning ( "%d points outside current region skip", outside_cnt );

    if ( nocat_cnt )
	G_warning ( "%d points without category skip", nocat_cnt );
    
    /* Sort cache by current region row */
    qsort (cache, point_cnt, sizeof (struct order), by_row);
      
    /* Allocate space for raster row */
    if ( out_type == CELL_TYPE ) 
        cell = G_allocate_c_raster_buf();
    else
	dcell = G_allocate_d_raster_buf();

    /* Extract raster values from file and store in cache */

    cur_row = -1;

    for (point = 0 ; point < point_cnt ; point++) {
	if ( cache[point].count > 1 ) continue; /* duplicate cats */

        if (cur_row != cache[point].row) {
	    if ( out_type == CELL_TYPE ) {
		if ( G_get_c_raster_row ( fd, cell, cache[point].row) < 0 )
		    G_fatal_error ( "Can't read raster" );
	    } else {
		if (G_get_d_raster_row (fd, dcell, cache[point].row) < 0)
		    G_fatal_error ( "Can't read raster" );
	    }
	}
	cur_row = cache[point].row;

	if ( out_type == CELL_TYPE ) {
	    cache[point].value = cell[ cache[point].col ];
	} else {
	    cache[point].dvalue = dcell[ cache[point].col ];
	}
    } /* point loop */

    /* Update table from cache */
    
    /* select existing categories to array (array is sorted) */
    select = db_select_int( driver, Fi->table, Fi->key, NULL, &catexst);
    
    db_begin_transaction ( driver );
    
    norec_cnt = update_cnt = upderr_cnt = dupl_cnt = 0;
    for (point = 0 ; point < point_cnt ; point++) {
	if ( cache[point].count > 1 ) {
	    G_warning ( "More points (%d) of category %d, value set to 'NULL'.", 
		           cache[point].count, cache[point].cat );
	    dupl_cnt++;
	}

	/* category exist in DB ? */
	cex = (int *) bsearch((void *) &(cache[point].cat), catexst, select, sizeof(int), srch_cat);
	if ( cex == NULL ) { /* cat does not exist in DB */ 
	    norec_cnt++;
	    G_warning ( "No record for category '%d' in the table", cache[point].cat );
	    continue;
	}

        sprintf ( buf, "update %s set %s = ", Fi->table, col_opt->answer);

	db_set_string ( &stmt, buf );
	
        if( out_type == CELL_TYPE) {
	    if ( cache[point].count > 1 || G_is_c_null_value ( &cache[point].value ) ) {
		sprintf ( buf, "NULL" );
	    } else {
	        sprintf ( buf, "%d ", cache[point].value);
	    }
	} else { /* FCELL or DCELL */
	    if( cache[point].count > 1 || G_is_d_null_value ( &cache[point].dvalue ) ) {
		sprintf ( buf, "NULL" );
	    } else {
	        sprintf ( buf,"%.10f", cache[point].dvalue);
	    }
        }
	db_append_string ( &stmt, buf );
	
	sprintf (buf, " where %s = %d", Fi->key, cache[point].cat);
	db_append_string ( &stmt, buf );
	
        G_debug ( 3, db_get_string (&stmt) );
	
	/* Update table */
	if ( db_execute_immediate (driver, &stmt) == DB_OK ){
	    update_cnt++;
	} else {    
	    upderr_cnt++;
	}
    }

    db_commit_transaction ( driver );
    free(catexst);	
    db_close_database_shutdown_driver ( driver );
    db_free_string (&stmt);

    /* Report */
    G_message ( _("%d categories loaded from table"), select );
    G_message ( _("%d categories loaded from vector"), point_cnt );
    G_message ( _("%d categories from vector missing in table"), norec_cnt );
    G_message ( _("%d duplicate categories in vector"), dupl_cnt );
    G_message ( _("%d records updated"), update_cnt );
    G_message ( _("%d update errors"), upderr_cnt );

    exit(0);
}

/* for qsort, order list by row */
static int by_row (const void *ii, const void *jj)
{
  const struct order *i = ii, *j = jj;
  return i->row - j->row;
}

/* for qsort, order list by cat */
static int by_cat (const void *ii, const void *jj)
{
  const struct order *i = ii, *j = jj;
  return i->cat - j->cat;
}

/* for bsearch, find cat */
static int srch_cat ( const void *pa, const void *pb)
{
    int       *p1 = (int *) pa;
    int       *p2 = (int *) pb;    

    if( *p1 < *p2 ) return -1;
    if( *p1 > *p2) return 1;
    return 0;
}  

