/* ****************************************************************************
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

/* Check if point is inside area with category of given field and returns 0 or category */
int point_area ( struct Map_info *Map, int field, double x, double y)
{
    int area, cat;
    
    area = Vect_find_area ( Map, x, y );
    G_debug (4, "  area = %d", area );

    if (!area) return 0;

    cat = Vect_get_area_cat ( Map, area, field );
    G_debug (4, "  cat = %d", cat );

    return cat;
}

int line_area ( struct Map_info *In, int *field, struct Map_info *Out, struct field_info *Fi, 
	        dbDriver *driver, int operator  )
{
    int    line, nlines, ncat;
    struct line_pnts *Points;
    struct line_cats *Cats;

    char     buf[1000];
    dbString stmt;

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();
    db_init_string (&stmt);

    /* Basic topology needed only */
    Vect_build_partial ( Out, GV_BUILD_BASE, NULL );

    nlines = Vect_get_num_lines ( Out );

    /* Warning!: cleaning process (break) creates new vertices which are usually slightly 
     * moved (RE), to compare such new vertex with original input is a problem?
     * 
     * TODO?: would it be better to copy centroids also and query output map? 
     */

    /* Check if the line is inside or outside binput area */
    ncat = 1;
    for ( line = 1; line <= nlines; line++ ) {
	int ltype, acat, lcat;


	G_percent ( line, nlines, 1 ); /* must be before any continue */
	
	if ( !Vect_line_alive(Out, line) ) continue;

	ltype = Vect_read_line ( Out, Points, Cats, line);
	
	if ( ltype ==  GV_BOUNDARY ) { /* No more needed */
	    Vect_delete_line ( Out, line );
	    continue;
	}

	/* Now the type should be only GV_LINE */

	/* Decide if the line is inside or outside the area. In theory:
	 * 1) All vertices outside
	 *      - easy, first vertex must be outside
	 * 2) All vertices inside 
	 * 3) All vertices on the boundary, we take it as inside (attention, 
	 *    result of Vect_point_in_area() for points on segments between vertices may be both
	 *    inside or outside, because of representation of numbers)
	 * 4) One or two end vertices on the boundary, all others outside
	 * 5) One or two end vertices on the boundary, all others inside 
	 *
	 */

	/* Note/TODO: the test done is quite simple, check the point in the middle of segment.
	 * If the line overpals the boundary, the result may be both outside and inside
	 * this should be solved (check angles?)
	 */
	
	G_debug (3, "line = %d", line );

	acat = point_area(&(In[1]), field[1], (Points->x[0]+Points->x[1])/2, (Points->y[0]+Points->y[1])/2);

	if ( (acat > 0 && operator == OP_AND) || (acat < 1 && operator == OP_NOT)  ) {
	    /* Point is inside */
	    G_debug (3, "  -> OK, write line" );
	    
	    /* rewrite with area cat */
	    Vect_cat_get(Cats, field[0], &lcat);
	    
	    Vect_reset_cats ( Cats );
	    Vect_cat_set (Cats, 1, ncat );
	    Vect_cat_set (Cats, 2, lcat);
	    if ( operator == OP_AND )
	        Vect_cat_set (Cats, 3, acat);
	    
	    Vect_rewrite_line ( Out, line, ltype, Points, Cats );

	    /* Attributes */
	    sprintf ( buf, "insert into %s values ( %d, %d", Fi->table, ncat, lcat ); 
	    db_set_string ( &stmt, buf);

	    if ( operator == OP_AND )
		 sprintf ( buf, ", %d )", acat );
	    else
		 sprintf ( buf, ")");
	    
	    db_append_string ( &stmt, buf);

	    G_debug ( 3, db_get_string ( &stmt ) );

	    if (db_execute_immediate (driver, &stmt) != DB_OK )
		G_warning ( "Cannot insert new row: %s", db_get_string ( &stmt ) );

	    ncat++;
	} else {
	    Vect_delete_line ( Out, line );
	}
    }

    return 0;
}

