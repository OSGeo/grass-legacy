/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes or Mike Higgins.
*               Update to GRASS 5.1 Radim Blazek and David D. Gray.
*
* PURPOSE:      Higher level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "gis.h"
#include "Vect.h"

extern FILE *Msgout;
extern int prnmsg ( char *msg, ...) ;

/*!
 \fn int Vect_build_shp ( struct Map_info *Map, FILE *msgout ) 
 \brief build topology SHAPE
 \return 1 on success, 0 on error
 \param Map_info structure, msgout - message output (stdout/stderr for example) or NULL
*/
int
Vect_build_shp ( struct Map_info *Map, FILE *msgout )
{
    struct Plus_head *plus ;
    int    i, n, offset, ret;
    int    line, type;
    int    area, isle;
    plus_t lines[1];
    struct line_pnts **Points, *CPoints;
    int    alloc_parts;
    int    *ptype, *pline;
    struct line_cats *Cats;
    P_LINE *Line;
    P_AREA *Area;
    P_ISLE *Isle;
    double area_size, x, y, fret;
    int     progress;
    int     nShapes, nParts, shape, part;
    int     first, last;
    SHPObject *pShape;
    BOUND_BOX box;
    
    plus = &(Map->plus);
    Msgout = msgout;
    nShapes = Map->fInfo.shp.nShapes;

    CPoints = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();

    Vect_rewind ( Map );
    
    if ( Map->fInfo.shp.type != SHPT_POLYGON  &&
         Map->fInfo.shp.type != SHPT_POLYGONZ &&
         Map->fInfo.shp.type != SHPT_POLYGONM ) 
    {
	/* Build points and lines */
        prnmsg ("Build topology for %d shapes:\n", nShapes );
        /* return ( Vect_build_nat ( Map, msgout) ); */
	
	while ( 1 ) {
	    G_debug ( 3, "register line" );
	    type = V1_read_next_line_shp (Map, CPoints, Cats);
	    if ( type == -1 ) {
		G_fatal_error ("Can't read data from vector '%s'",Vect_get_full_name(Map));
	    } else if ( type == -2 ) {
	        break;
            }
            offset = Vect_last_line_offset (Map);
	    G_debug ( 3, "Register line: offset = %d", offset );
	    line = dig_add_line ( plus, type, CPoints, offset ); 
	    G_debug ( 3, "Line registered with line = %d", line );
	    
            /* Set box */
	    dig_line_box ( CPoints, &box );
            if ( line == 1 ) Vect_box_copy (&(plus->box), &box);
            else Vect_box_extend (&(plus->box), &box);
	}	
    } 
    else 
    {
	G_warning ("Topology for shapefile areas is not fully supported.\n");
    
      
	alloc_parts = 10; 
	Points = (struct line_pnts **) G_malloc ( alloc_parts * sizeof ( struct line_pnts *) );
	ptype = (int *) G_malloc ( alloc_parts * sizeof ( int *) );
	pline = (int *) G_malloc ( alloc_parts * sizeof ( int *) );
	for ( i = 0; i < alloc_parts; i++ ) {
	    Points[i] = Vect_new_line_struct (); 
	}
	
	prnmsg ("Registering areas: ");
	for ( shape = 0; shape < nShapes; shape++ ) {
	    G_debug ( 3, "shape = %d", shape );
	    pShape = SHPReadObject( Map->fInfo.shp.hShp, shape );
	    nParts = pShape->nParts;
	
	    if ( nParts > alloc_parts ) {
		Points = (struct line_pnts **) G_realloc ( (void *) Points, nParts * sizeof ( struct line_pnts *) );
		ptype = (int *) G_realloc ( (void *) ptype, nParts * sizeof ( int *) );
		pline = (int *) G_realloc ( (void *) pline, nParts * sizeof ( int *) );
		for ( i = alloc_parts; i < nParts; i++ ) {
		    Points[i] = Vect_new_line_struct (); 
		}
		alloc_parts = nParts;
	    }
		
	    /* Read all parts to array */
	    for ( part = 0; part < nParts; part++ ) {
		first = pShape->panPartStart[part];
		if( part == pShape->nParts - 1 ) {
		    last = pShape->nVertices - 1;
		} else {
		    last = pShape->panPartStart[part+1] - 1;
		}
		G_debug ( 3, "part = %d from %d to %d", part, first, last );
		Vect_reset_line ( Points[part] );
		for ( i = first; i <= last; i++ ) {
		    /* TODO do it better (speed) */
		    Vect_append_point ( Points[part], pShape->padfX[i], pShape->padfY[i], 0 );
		}
		G_debug ( 3, "   -> n_points = %d",  Points[part]->n_points );
		
		/* register line */
		offset = ( ( shape << 11 ) | ( part & 0x7FF) );
		G_debug ( 3, "Register line: offset = %d", offset );
		line = dig_add_line ( plus, GV_BOUNDARY, Points[part], offset );
		pline[part] = line;
		dig_line_box ( Points[part], &box );
		dig_line_set_box (plus, line, &box);
		if ( line == 1 )
		    Vect_box_copy (&(plus->box), &box);
		else
		    Vect_box_extend (&(plus->box), &box);
				
		/* Check part type: area or isle */
		dig_find_area_poly (Points[part], &area_size);
		G_debug ( 3, "  area/isle size = %f", area_size );

		if (area_size > 0) {  /* area */
		    ptype[part] = 1;
		    G_debug ( 3, "  -> area" );
		} else if (area_size < 0) { /* island */
		    G_debug ( 3, "  -> isle" );
		    ptype[part] = 2;
		}
	    }
	    SHPDestroyObject(pShape);

	    /* Go through parts again attach isles and register */
	    for ( part = 0; part < nParts; part++ ) {
		G_debug ( 3, "  part = %d ptype = %d", part, ptype[part] );
		if ( ptype[part] == 1 ) { /* area */
		    /* register area */
		    lines[0] = pline[part]; 
		    area = dig_add_area (plus, 1, lines);
		    dig_line_box ( Points[part], &box );
		    dig_area_set_box (plus, area, &box);
		    
		    /* find islands inside area */
		    for ( i = 0; i < nParts; i++ ) {
			if ( ptype[i] == 2 ) { /* not registerd island */
			    fret = dig_point_in_poly ( Points[i]->x[0], Points[i]->y[0], Points[part]);
			    G_debug ( 3, "isle in area ? = %f", fret );
			    if ( fret > 0 ) { /* isle inside area */
				G_debug ( 3, "isle (part = %d) in area", i );
				/* register island */ 
				lines[0] = pline[i]; 
				isle = dig_add_isle (plus, 1, lines);
				dig_line_box ( Points[i], &box );
				dig_isle_set_box (plus, isle, &box);
				
				G_debug ( 3, " -> isle %d", isle );
				Isle = plus->Isle[isle];
				Isle->area = area;
				
				Area = plus->Area[area];
				n = Area->n_isles;
				if ( dig_area_alloc_isle (Area, 1) == -1 )
				    return 0;
				
				Area->isles[n] = isle;
				Area->n_isles++;
				G_debug ( 3, "n_isles = %d", Area->n_isles);
				ptype[i] = 0; /* used */
			    } else { 
				G_debug ( 3, "isle (part = %d) is not in area %d", i, area );
			    }
			}

		    }

		    /* create virtual centroid */
		    /* !! offset for virtual centroids is offset for part 0 */
		    /* TODO calculate better centroid coordinates */
		    offset =  ( shape << 11 ) ;
		    Vect_reset_line ( CPoints );
		    ret = Vect_get_point_in_area ( Map, area, &x, &y ); 
		    if ( ret < -1 ) {
			G_warning ( "Cannot calculate centroid for shape area (area %d, shape %d, part %d)",
				                area, shape, part );
			x = Points[part]->x[0];
			y = Points[part]->y[0];
		    }
		    Vect_append_point ( CPoints, x, y, 0 );
		    line = dig_add_line ( plus, GV_CENTROID, CPoints, offset );
		    dig_line_box ( CPoints, &box );
		    dig_line_set_box (plus, line, &box);
		    
		    Line = plus->Line[line];
		    Line->left = area;

		    /* register centroid to area */
		    Area = plus->Area[area];
		    Area->centroid = line;
		}
	    }
	    
	    /* print progress */
	    progress = ( int ) 100 *  (shape + 1) / nShapes;  
	    prnmsg ("%4d%%\b\b\b\b\b", progress);
	}
    }
    
    return 1;
}


