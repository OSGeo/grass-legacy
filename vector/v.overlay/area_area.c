/*  $Id$
 *  ****************************************************************************
 *  
 *  MODULE: v.overlay 
 *
 *  AUTHOR(S): Radim Blazek
 *  
 ******************************************************************************/
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "local.h"

int area_area ( struct Map_info *In, int *field, struct Map_info *Out, struct field_info *Fi, 
	        dbDriver *driver, int operator  )
{
    int    ret, input, line, nlines, area, nareas;
    int    in_area, in_centr, in_cat;
    struct line_pnts *Points;
    struct line_cats *Cats;
    CENTR  *Centr;
    char    *Del;

    char     buf[1000];
    dbString stmt;

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();

    /* Probably not necessary for LINE x AREA */
    fprintf (stderr, SEP );
    fprintf ( stderr, "Removing duplicates ...\n" );
    Vect_remove_duplicates ( Out, GV_BOUNDARY, NULL, stderr );

    /* ?: May be result of Vect_break_lines() + Vect_remove_duplicates() any dangle or bridge?
     * In that case, calls to Vect_remove_dangles() and Vect_remove_bridges() would be also necessary */
    
    /* Attach islands */
    fprintf (stderr, SEP );
    fprintf ( stderr, "Attaching islands ...\n" );
    Vect_build_partial ( Out, GV_BUILD_ATTACH_ISLES, stderr );


    /* Calculate new centroids for all areas */
    nareas = Vect_get_num_areas ( Out );

    Centr = (CENTR *) G_malloc ( (nareas+1) * sizeof ( CENTR ) ); /* index from 1 ! */
    for ( area = 1; area <= nareas; area++ ) { 
	ret = Vect_get_point_in_area ( Out, area, &(Centr[area].x), &(Centr[area].y) );
	if ( ret < 0 ) {
	    G_warning ("Cannot calculate area centroid" );
	    Centr[area].valid = 0;
	} else {
	    Centr[area].valid = 1;
	}
    }
    
    /* Query input maps */
    for ( input = 0; input < 2; input++ ) {
        fprintf (stderr, SEP );
	fprintf (stderr, "Querying input '%s' ... ", Vect_get_full_name(&(In[input])) );

	for ( area = 1; area <= nareas; area++ ) {
	    Centr[area].cat[input] = 0;
	    
	    in_area = Vect_find_area ( &(In[input]), Centr[area].x, Centr[area].y );
	    if ( in_area > 0 ) {
		in_centr = Vect_get_area_centroid (  &(In[input]), in_area );
		if ( in_centr > 0 ) {
	            Vect_read_line ( &(In[input]), NULL, Cats, in_centr);
		    Vect_cat_get (Cats, field[input], &in_cat);
		    Centr[area].cat[input] = in_cat;
		}
	    }
	    G_percent ( area, nareas, 1 );
	}
    }

    fprintf (stderr, SEP );
    fprintf (stderr, "Writing centroids ...\n" );
    
    for ( area = 1; area <= nareas; area++ ) {
	/* check the condition */
        switch (operator) {
	    case OP_AND:
		if ( !( Centr[area].cat[0] && Centr[area].cat[1] ) ) continue;
		break;
	    case OP_OR:
		if ( !( Centr[area].cat[0] || Centr[area].cat[1] ) ) continue;
		break;
	    case OP_NOT:
		if ( !( Centr[area].cat[0] && !(Centr[area].cat[1]) ) ) continue;
		break;
	    case OP_XOR:
		if ( (Centr[area].cat[0] && Centr[area].cat[1]) ||
		     ( !(Centr[area].cat[0]) && !(Centr[area].cat[1]) ) ) continue;
		break;
	}
	
	Vect_reset_line ( Points );
        Vect_reset_cats ( Cats );

	Vect_append_point ( Points, Centr[area].x, Centr[area].y, 0.0 );
	Vect_cat_set (Cats, 1, area);
	
	if ( Centr[area].cat[0] > 0 )
	    Vect_cat_set (Cats, 2, Centr[area].cat[0]);
	
	if ( Centr[area].cat[1] > 0 )
	    Vect_cat_set (Cats, 3, Centr[area].cat[1]);

	Vect_write_line ( Out, GV_CENTROID, Points, Cats );
	
	G_percent ( area, nareas, 1 );
    }

    /* Build topology and remove boundaries with area without centroid on both sides */
    fprintf (stderr, SEP );
    fprintf ( stderr, "Attaching centroids ...\n" );
    Vect_build_partial ( Out, GV_BUILD_ALL, stderr );

    /* Create a list of lines to be deleted */
    nlines = Vect_get_num_lines ( Out );
    Del = (char *) G_calloc ( nlines+1,  sizeof(char) ); /* index from 1 ! */

    for ( line = 1; line <= nlines; line++ ) {
	int i, ltype, side[2], centr[2];
	G_percent ( line, nlines, 1 ); /* must be before any continue */
	
	if ( !Vect_line_alive(Out, line) ) continue;

	ltype = Vect_read_line ( Out, NULL, NULL, line);
	if ( !(ltype & GV_BOUNDARY) ) continue;
	
	Vect_get_line_areas ( Out, line, &side[0], &side[1] );

	for ( i = 0; i < 2; i++ ) {
	    if ( side[i] == 0 ) { /* This should not happen ! */
		centr[i] = 0;
		continue;
	    }

	    if ( side[i] > 0 ) {
		area = side[i]; 
	    } else { /* island */
		area = Vect_get_isle_area ( Out, abs ( side[i] ) ); 
	    }

	    if ( area > 0 )
		centr[i] = Vect_get_area_centroid ( Out, area );
	    else 
	        centr[i] = 0;
	}
	
	if ( !centr[0] && !centr[1] ) Del[line] = 1;
    }

    /* Delete boundaries */
    for ( line = 1; line <= nlines; line++ ) {
	if ( Del[line] )
	    Vect_delete_line ( Out, line );
    }
    G_free ( Del );

    /* attributes */
    if ( driver ) {
	fprintf (stderr, SEP );
	fprintf ( stderr, "Writing attributes ...\n" );

	db_init_string (&stmt);
	
	for ( area = 1; area <= nareas; area++ ) {
	    switch (operator) {
		case OP_AND:
		    if ( !( Centr[area].cat[0] && Centr[area].cat[1] ) ) continue;
		    break;
		case OP_OR:
		    if ( !( Centr[area].cat[0] || Centr[area].cat[1] ) ) continue;
		    break;
		case OP_NOT:
		    if ( !( Centr[area].cat[0] && !(Centr[area].cat[1]) ) ) continue;
		    break;
		case OP_XOR:
		    if ( (Centr[area].cat[0] && Centr[area].cat[1]) ||
			 ( !(Centr[area].cat[0]) && !(Centr[area].cat[1]) ) ) continue;
		    break;
	    }

	    sprintf ( buf, "insert into %s values ( %d", Fi->table, area ); 
	    db_set_string ( &stmt, buf);

	    /* cata */
	    if ( Centr[area].cat[0] > 0 )
		 sprintf ( buf, ", %d", Centr[area].cat[0] );
	    else
		 sprintf ( buf, ", null");
	    
	    db_append_string ( &stmt, buf);

	    /* catb */
	    if ( Centr[area].cat[1] > 0 )
		 sprintf ( buf, ", %d )", Centr[area].cat[1] );
	    else
		 sprintf ( buf, ", null )");
	    
	    db_append_string ( &stmt, buf);

	    G_debug ( 3, db_get_string ( &stmt ) );

	    if (db_execute_immediate (driver, &stmt) != DB_OK )
		G_warning ( "Cannot insert new row: %s", db_get_string ( &stmt ) );
	
	    G_percent ( area, nareas, 1 );
	}	
    }
    
    return 0;
}

