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
    int    i, j, k, s, n, type, node, lineid, offset, ret;
    int    n_lines, side, line, bline;
    int    area, isle, direction, sel_area;
    int    found, part, first;
    plus_t *lines;
    struct line_pnts *Points, *APoints;
    struct line_cats *Cats;
    P_LINE *Line, *BLine;
    P_NODE *Node;
    P_AREA *Area;
    P_ISLE *Isle;
    double  area_size, dist, cur_dist;
    int     progress, last_progress;
    BOUND_BOX box;
    struct ilist *List;
    
    plus = &(Map->plus);
    Msgout = msgout;
    
    Points = Vect_new_line_struct ();
    APoints = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    List = Vect_new_list ();

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
	dig_line_box ( Points, &box );
        dig_line_set_box (plus, lineid, &box);  
	if ( lineid == 1 ) 
	    Vect_box_copy (&(plus->box), &box);
        else
	    Vect_box_extend (&(plus->box), &box);
	
	/* print progress */
	if ( i == 10 ) {
            prnmsg ("%7d\b\b\b\b\b\b\b", j);
	    i = 0;
	}
	i++; j++;
    }
    prnmsg ("       \n%d lines registered\n", plus->n_lines);

    /* Build areas */
    /* Go through all bundaries and try to build area for both sides */
    prnmsg ("Building areas: ");
    last_progress = -1; 
    for (i = 1; i <= plus->n_lines; i++) {
	/* print progress */
        progress = ( int ) 100 *  i / plus->n_lines;
	if ( progress > last_progress ) {
            prnmsg ("%4d%%\b\b\b\b\b", progress);
	    last_progress = progress;
	}

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
                type = Vect_read_line (Map, Points, Cats, line );
		if ( lines[j] > 0 ) direction = GV_FORWARD; 
		else direction = GV_BACKWORD;
	        Vect_append_points ( APoints, Points, direction);
	    }
	    
	    dig_find_area_poly (APoints, &area_size);
	    G_debug ( 3, "  area/isle size = %f", area_size );
            
	    /* TODO calculate box from boxes of boundaries (speed) */
	    dig_line_box ( APoints, &box );

	    if (area_size > 0) {  /* area */
                /* add area structure to plus */
	        area = dig_add_area (plus, n_lines, lines);
                dig_area_set_box (plus, area, &box);  
                G_debug ( 3, "  -> area %d", area );
	    } else if (area_size < 0) { /* island */
	        isle = dig_add_isle (plus, n_lines, lines);
                dig_isle_set_box (plus, isle, &box);  
                G_debug ( 3, "  -> isle %d", isle );
	    } else {
		G_warning ("area size = 0"); 
	    }
	}
    }
    prnmsg ("\n%d areas built\n%d isles built\n", plus->n_areas, plus->n_isles );

    /* Attache isles to areas */
    prnmsg ("Attaching islands: ");
    last_progress = -1; 
    for (i = 1; i <= plus->n_isles; i++) {
        Isle = plus->Isle[i];
        line = abs(Isle->lines[0]);
	Line = plus->Line[line];
        node = Line->N1;
	Node = plus->Node[node];
	
        G_debug ( 3, "island = %d (node = %d)", i, node );
	/* select areas by box */
	box.E = Node->x;
	box.W = Node->x;
	box.N = Node->y;
	box.S = Node->y;
	box.T = PORT_DOUBLE_MAX;
	box.B = -PORT_DOUBLE_MAX;
	Vect_select_areas_by_box (Map, &box, List);
        G_debug ( 3, "%d areas overlap island box", List->n_values );
	sel_area = 0;
	first = 1;
        for (j = 0; j < List->n_values; j++) {
	    area = List->value[j];
            G_debug ( 3, "area = %d", area );

	    Vect_get_area_points (Map, area, APoints);
            cur_dist = dig_point_in_poly ( Node->x, Node->y, APoints);
            G_debug ( 3, "current dist = %f", cur_dist );
	    
	    if (cur_dist > 0) { /* point in area but area may be part of isle */
		part = 0;
		for ( k = 0; k < Node->n_lines; k++ ) {
		    bline = abs ( Node->lines[k] );
		    BLine = plus->Line[bline];
		    if ( BLine->left == area || BLine->right == area ) {
			/* -> this area is part of isle */
			part = 1;
			break;
		    }
		}
		if ( !part ) { /* node in area which is not part of isle */
                    if ( first ) {
			sel_area = area;
		        dist = cur_dist;
			first = 0;
		    } else {
			if ( cur_dist < dist ) {
			    sel_area = area;
			    dist = cur_dist;
			}
		    }
		}
	    }
	}
        if ( sel_area > 0 ) {	
	    G_debug ( 3, "Island %d in area %d", i, sel_area );
	    Isle->area = sel_area;
	    
	    Area = plus->Area[sel_area];
	    n = Area->n_isles;
	    if ( dig_area_alloc_isle (Area, 1) == -1 )
		return 0;
	    
	    Area->isles[n] = i;
	    Area->n_isles++;
	    G_debug ( 3, "n_isles = %d", Area->n_isles);
	} else { 
	    G_debug ( 3, "Island %d is not in area" );
	}
	/* print progress */
        progress = ( int ) 100 *  i / plus->n_isles;  
	if ( progress > last_progress ) {
            prnmsg ("%4d%%\b\b\b\b\b", progress);
	    last_progress = progress;
	}
    }
    
    /* Attache centroids to areas */
    prnmsg ("\nAttaching centroids: ");
    last_progress = -1; 
    for (area = 1; area <= plus->n_areas; area++) {
	/* print progress */
        progress = ( int ) 100 *  area / plus->n_areas;  
	if ( progress > last_progress ) {
            prnmsg ("%4d%%\b\b\b\b\b", progress);
	    last_progress = progress;
	}
	
	/* attache */
	Area = plus->Area[area];
	Vect_get_area_box ( Map, area, &box );
	box.T = PORT_DOUBLE_MAX;
	box.B = -PORT_DOUBLE_MAX;
	Vect_select_lines_by_box (Map, &box, GV_CENTROID, List);
        G_debug ( 3, "%d centroids in area box", List->n_values );

	found = 0;
        for (i = 0; i < List->n_values; i++) {
	    line = List->value[i];
            G_debug ( 3, "line  = %d", line );
	    
	    Line = plus->Line[line];
	    Node = plus->Node[Line->N1];
	    
	    ret = Vect_point_in_area (Map, area, Node->x, Node->y);
	    if ( ret ) {
                G_debug ( 3, "Centroid (line=%d) in area %d", i, area );
	        if ( found == 0  ) {
		    Area->centroid = line;
		    Line->left = area;
		}
		found++;
		if ( found == 2 ) 
                    prnmsg ("\n");
		
		if ( found > 1 ) { 
		    Line->left = -area;
 		    G_warning ( "%d. centroid found in area %d", found, area );
		}
	    }
	}
	if ( found > 1 ) 
	    prnmsg ("Attaching centroids: ");
	    
	//if( found == 0 )
	//    G_warning ("No centroid in area %d", area);
    }
    prnmsg ("\n");

    return 1;
}


