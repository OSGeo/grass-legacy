/*
* $Id$
*
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

/* Build topology 
*  msgout - message output (stdout/stderr for example) or NULL
*
*  Returns: 1 - success
*           0 - error
*/
int
Vect_build_nat ( struct Map_info *Map, FILE *msgout ) {
    struct Plus_head *plus ;
    int    i, j, s, n, type, n_points, node, lineid, offset, ret;
    int    n_lines, side, line;
    int    area, isle, direction;
    int    found;
    plus_t *lines;
    struct line_pnts *Points, *APoints;
    struct line_cats *Cats;
    P_LINE *Line, *BLine;
    P_NODE *Node;
    P_AREA *Area;
    P_ISLE *Isle;
    double area_size, poly;
    int     progress;
    
    plus = &(Map->plus);
    Msgout = msgout;
    
    Points = Vect_new_line_struct ();
    APoints = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();

    /* 
    *  We shall go through all primitives in coor file and 
    *  add new node for each end point to nodes structure
    *  if the node with the same coordinates doesn't exist yet.
    */
   
    /* register lines, create nodes */ 
    
    Vect_rewind ( Map );
    prnmsg ("Registering lines: ");
    i = 1; j = 1;
    while ( 1 ) {
	/* register line */
	offset = Vect_next_line_offset (Map);
        type = Vect_read_next_line (Map, Points, Cats);
        if ( type == -1 ) {
	    fprintf (stderr, "\nERROR: vector file - can't read\n" );
	    return 0;
        } else if ( type == -2 ) {
	    break;
	}
	G_debug ( 3, "Register line: offset = %d", offset );
	lineid = dig_add_line ( plus, type, Points, offset );
	/* print progress */
	if ( i == 10 ) {
            prnmsg ("%6d\b\b\b\b\b\b", j);
	    i = 0;
	}
	i++; j++;
    }
    prnmsg ("\n%d lines registered                 \n", plus->n_lines);

    /* Build areas */
    /* Go through all bundaries and try to build area for both sides */
    prnmsg ("Building areas: ");
    
    for (i = 1; i <= plus->n_lines; i++) {
	/* print progress */
        progress = ( int ) 100 *  i / plus->n_lines;  
        prnmsg ("%4d%%\b\b\b\b\b", progress);

	/* build */
	if ( plus->Line[i] == NULL ) { continue; } /* dead line */
	Line = plus->Line[i];
	if ( Line->type != GV_BOUNDARY ) { continue; }

        side = GV_LEFT;
        for (s = 0; s < 2; s++) {
	    if ( s == 1 ) { side = GV_RIGHT; }
	   
	    G_debug ( 3, "Build area for lineid = %d, side = %d", i, side );
	    
	    area = dig_line_get_area (plus, i, side); 
	    if ( area != 0 ) {
	        G_debug ( 3, "  area/isle = %d -> skip", area );
		continue;
	    }
	    
	    n_lines = dig_build_area_with_line (plus, i, side, &lines);
	    G_debug ( 3, "  n_lines = %d", n_lines );
	    if ( n_lines < 1 ) { continue; } /* area was not built */

	    /* Area or island ? */
	    Vect_reset_line ( APoints );
	    for (j = 0; j < n_lines; j++){
		line = abs(lines[j]);
		BLine = plus->Line[line];
		offset = BLine->offset;
		G_debug ( 3, "  line[%d] = %d, offset = %d", j, line, offset );
                type = V2_read_line (Map, Points, Cats, line );
		if ( lines[j] > 0 ) direction = GV_FORWARD; 
		else direction = GV_BACKWORD;
	        Vect_append_points ( APoints, Points, direction);
	    }
	    
	    dig_find_area_poly (APoints, &area_size);
            G_debug ( 3, "  area/isle size = %f", area_size );

	    if (area_size > 0) {  /* area */
                /* add area structure to plus */
                G_debug ( 3, "  -> area %d", area );
	        area = dig_add_area (plus, n_lines, lines);
	    } else if (area_size < 0) { /* island */
                G_debug ( 3, "  -> isle %d", isle );
	        isle = dig_add_isle (plus, n_lines, lines);
	    } else {
		G_warning ("area size = 0"); 
	    }
	}
    }
    prnmsg ("\n%d areas built\n%d isles built\n", plus->n_areas, plus->n_isles );

    /* Attache isles to areas */
    prnmsg ("Attaching islands: ");
    for (i = 1; i <= plus->n_isles; i++) {
        Isle = plus->Isle[i];
        line = abs(Isle->lines[0]);
	Line = plus->Line[line];
        node = Line->N1;
	Node = plus->Node[line];
	
        G_debug ( 3, "island = %d", i );
        for (j = 1; j <= plus->n_areas; j++) {
	    /* skip area inside island - is it all right ?? */
	    if ( abs(Line->left) == j || abs(Line->right) == j ) continue;
	    Vect_get_area_points (Map, j, APoints);
            poly = dig_point_in_poly ( Node->x, Node->y, APoints);
            G_debug ( 3, "poly = %f", poly );
	    /* TODO not first but smallest */
	    if (poly > 0) {
                G_debug ( 3, "Island %d in area %d", i, j );
		Isle->area = j;
		
		Area = plus->Area[j];
		n = Area->n_isles;
                if ( dig_area_alloc_isle (Area, 1) == -1 )
                    return 0;
		
		Area->isles[n] = i;
		Area->n_isles++;
                G_debug ( 3, "n_isles = %d", Area->n_isles);
		break;
	    }
	}
	/* print progress */
        progress = ( int ) 100 *  i / plus->n_isles;  
        prnmsg ("%4d%%\b\b\b\b\b", progress);
    }
    
    /* Attache centroids to areas */
    prnmsg ("\nAttaching centroids: ");
    for (i = 1; i <= plus->n_lines; i++) {
	/* print progress */
        progress = ( int ) 100 *  i / plus->n_lines;  
        prnmsg ("%4d%%\b\b\b\b\b", progress);
	
	/* attache */
	Line = plus->Line[i];
	if ( Line->type != GV_CENTROID ) { continue; }
        
	Node = plus->Node[Line->N1];
	found = 0;
        for (j = 1; j <= plus->n_areas; j++) {
	    ret = Vect_point_in_area (Map, j, Node->x, Node->y);
	    if ( ret ) {
                G_debug ( 3, "Centroid (line=%d) in area %d", i, j );
		Area = plus->Area[j];
		n = Area->n_centroids;
                if ( dig_area_alloc_centroid (Area, 1) == -1 )
                    return 0;
		
		Area->centroids[n] = i;
		Area->n_centroids++;
                G_debug ( 3, "n_centroids = %d", Area->n_centroids);
		found = 1;
		break;
	    }
	}
	if(!found)
	    G_warning ("Centroid (line=%d, %f, %f) outside area", i, Node->x, Node->y);
    }

    return 1;
}


