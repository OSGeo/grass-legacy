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
 \fn int Vect_build_line_area ( struct Map_info *Map, int iline, int side )
 \brief build area on given side of line ( GV_LEFT or GV_RIGHT )
 \return > 0 : number of  area, < 0 : number of isle, 0 : not created (may also already exist)
 \param Map_info structure, line number ?, side (left? right?)
*/
int
Vect_build_line_area ( struct Map_info *Map, int iline, int side )
{
    int    j, area, isle, n_lines, line, type, direction;
    long   offset;
    struct Plus_head *plus ;
    P_LINE *BLine;
    struct line_pnts *Points, *APoints;
    plus_t *lines;
    double area_size;

    plus = &(Map->plus);

    G_debug ( 3, "Vect_build_line_area() line = %d, side = %d", iline, side );
    
    Points = Vect_new_line_struct ();
    APoints = Vect_new_line_struct ();

    area = dig_line_get_area (plus, iline, side); 
    if ( area != 0 ) {
	G_debug ( 3, "  area/isle = %d -> skip", area );
	return 0;
    }
    
    n_lines = dig_build_area_with_line (plus, iline, side, &lines);
    G_debug ( 3, "  n_lines = %d", n_lines );
    if ( n_lines < 1 ) { return 0; } /* area was not built */

    /* Area or island ? */
    Vect_reset_line ( APoints ); 
    for (j = 0; j < n_lines; j++){
	line = abs(lines[j]);
	BLine = plus->Line[line];
	offset = BLine->offset;
	G_debug ( 3, "  line[%d] = %d, offset = %d", j, line, offset );
	type = Vect_read_line (Map, Points, NULL, line );
	if ( lines[j] > 0 ) direction = GV_FORWARD; 
	else direction = GV_BACKWORD;
	Vect_append_points ( APoints, Points, direction);
    }
    
    dig_find_area_poly (APoints, &area_size);
    G_debug ( 3, "  area/isle size = %f", area_size );
    
    if (area_size > 0) {  /* area */
	/* add area structure to plus */
	area = dig_add_area (plus, n_lines, lines);
	if ( area == -1 ) { /* error */
	    Vect_close ( Map );
	    G_fatal_error ( "Cannot add area (map closed, topo saved)" );
	}
	G_debug ( 3, "  -> area %d", area );
	return area;
    } else if (area_size < 0) { /* island */
	isle = dig_add_isle (plus, n_lines, lines);
	if ( isle == -1 ) { /* error */
	    Vect_close ( Map );
	    G_fatal_error ( "Cannot add isle (map closed, topo saved)" );
	}
	G_debug ( 3, "  -> isle %d", isle );
	return -isle;
    } else { 
	/* TODO: What to do with such areas? Should be areas/isles of size 0 stored,
	*        so that may be found and cleaned by some utility
	*  Note: it would be useful for vertical closed polygons, but such would be added twice
	*        as area */
	G_warning ("Area of size = 0.0 ignored"); 
    }
    return 0;
}

/*!
 \fn int Vect_isle_find_area ( struct Map_info *Map, int isle ) 
 \brief find area outside island
 \return number of  area(s), 0 if not found
 \param Map_info structure, isle number
*/
int
Vect_isle_find_area ( struct Map_info *Map, int isle ) 
{
    int    j, k, line, node, sel_area, first, area, part, bline;
    struct Plus_head *plus ;
    P_LINE *Line, *BLine;
    P_NODE *Node;
    P_ISLE *Isle;
    double  dist, cur_dist;
    BOUND_BOX box;
    struct ilist *List;
    struct line_pnts *Points, *APoints;

    /* Note: We should check all isle points (at least) because if topology is not clean
    *  and two areas overlap, isle which is not completely within area may be attached,
    *  but it would take long time */
    G_debug ( 3, "Vect_isle_find_area () island = %d", isle );
    plus = &(Map->plus);
    List = Vect_new_list ();
    
    Points = Vect_new_line_struct ();
    APoints = Vect_new_line_struct ();

    if (  plus->Isle[isle] == NULL ) {
	G_warning ("Request to find area outside nonexisting isle");
	return 0;
    }
    Isle = plus->Isle[isle];
    line = abs(Isle->lines[0]);
    Line = plus->Line[line];
    node = Line->N1;
    Node = plus->Node[node];
    
    /* select areas by box */
    box.E = Node->x; box.W = Node->x; box.N = Node->y; box.S = Node->y;
    box.T = PORT_DOUBLE_MAX; box.B = -PORT_DOUBLE_MAX;
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
	G_debug ( 3, "Island %d in area %d", isle, sel_area );
    } else { 
	G_debug ( 3, "Island %d is not in area", isle );
    }

    return sel_area;
}

/*!
 \fn int Vect_attach_isle ( struct Map_info *Map, int isle ) 
 \brief (Re)Attach isle to area
 \return ? not sure yet what should be returned
 \param Map_info structure, isle number
*/
int
Vect_attach_isle ( struct Map_info *Map, int isle ) 
{
    int sel_area;
    P_ISLE *Isle;
    struct Plus_head *plus ;

    /* Note!: If topology is not clean and areas overlap, one island may fall to more areas
    *  (partially or fully). Before isle is attached to area it must be check if it is not attached yet */
    G_debug ( 3, "Vect_attach_isle (): isle = %d", isle);

    plus = &(Map->plus);
    
    sel_area = Vect_isle_find_area ( Map, isle );
    G_debug ( 3, "      isle = %d -> area outside = %d", isle, sel_area);
    if ( sel_area > 0 ) {
	Isle = plus->Isle[isle];
	if ( Isle->area > 0 ) {
	    G_debug (3, "Attempt to attach isle %d to more areas (=>topology is not clean)");
	} else {
	    Isle->area = sel_area;
	    dig_area_add_isle ( plus, sel_area, isle );
	}
    }
    return 0;
}

/*!
 \fn int Vect_attach_isles ( struct Map_info *Map, BOUND_BOX *box )
 \brief (Re)Attach isles to areas in given box
 \return ? not sure yet what should be returned
 \param Map_info structure, BOUND_BOX
*/
int
Vect_attach_isles ( struct Map_info *Map, BOUND_BOX *box )
{
    int i, isle;
    struct ilist *List;
    struct Plus_head *plus ;

    G_debug ( 3, "Vect_attach_isles ()");

    plus = &(Map->plus);
    List = Vect_new_list (); 
    
    Vect_select_isles_by_box ( Map, box, List);
    G_debug ( 3, "  number of isles to attach = %d", List->n_values);

    for (i = 0; i < List->n_values; i++) { 
	isle = List->value[i];
        Vect_attach_isle ( Map, isle );
    }
    return 0;
}

/*!
 \fn int Vect_attach_centroids ( struct Map_info *Map, BOUND_BOX *box ) 
 \brief (Re)Attach centroids to areas in given box
 \return ? not sure yet what should be returned
 \param Map_info structure, BOUND_BOX
*/
int
Vect_attach_centroids ( struct Map_info *Map, BOUND_BOX *box ) 
{
    int i, sel_area, centr;
    struct ilist *List;
    P_AREA *Area;
    P_NODE *Node;
    P_LINE *Line;
    struct Plus_head *plus ;

    G_debug ( 3, "Vect_attach_centroids ()");
    
    plus = &(Map->plus);
    List = Vect_new_list (); 
    
    Vect_select_lines_by_box ( Map, box, GV_CENTROID, List);
    G_debug ( 3, "  number of centroids to reattach = %d", List->n_values);
    for (i = 0; i < List->n_values; i++) { 
	centr = List->value[i];
	Line = plus->Line[centr];
	Node = plus->Node[Line->N1];
	sel_area = Vect_find_area ( Map, Node->x, Node->y );
	G_debug ( 3, "  centroid %d is in area %d", centr, sel_area);
	if ( sel_area > 0 ) {
	    Area = plus->Area[sel_area];
	    if ( Area->centroid == 0 ) { /* first centroid */
		G_debug ( 3, "  first centroid -> attach to area");
		Area->centroid = centr;
		Line->left = sel_area;
	    } else {  /* duplicate centroid */
		G_debug ( 3, "  duplicate centroid -> do not attach to area");
		Line->left = -sel_area;
	    }
	}
    }
    
    return 0;
}
    
/*!
 \fn int Vect_build_nat ( struct Map_info *Map, FILE *msgout )
 \brief build topology 
 \return 1 on success, 0 on error
 \param Map_info structure, msgout - message output (stdout/stderr for example) or NULL
*/
int
Vect_build_nat ( struct Map_info *Map, FILE *msgout )
{
    struct Plus_head *plus ;
    int    i, j, s, type, lineid, offset, ret;
    int    side, line, area, found;
    int     progress, last_progress;
    struct line_pnts *Points, *APoints;
    struct line_cats *Cats;
    P_LINE *Line;
    P_NODE *Node;
    P_AREA *Area;
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
        type = V1_read_next_line_nat (Map, Points, Cats);
	/* Note: check for dead lines is not needed, because they are skipped by V1_read_next_line_nat() */
	if ( type == -1 ) { 
	    fprintf (stderr, "\nERROR: vector file - can't read\n" );
	    return 0;
        } else if ( type == -2 ) {
	    break;
	}
	offset = Vect_last_line_offset (Map);
	G_debug ( 3, "Register line: offset = %d", offset );
	lineid = dig_add_line ( plus, type, Points, offset );
	dig_line_box ( Points, &box );
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

        for (s = 0; s < 2; s++) {
            if ( s == 0 ) side = GV_LEFT;
	    else side = GV_RIGHT;
	   
	    G_debug ( 3, "Build area for line = %d, side = %d", i, side );
            Vect_build_line_area ( Map, i, side );
	}
    }
    prnmsg ("\n%d areas built\n%d isles built\n", plus->n_areas, plus->n_isles );

    /* Attach isles to areas */
    prnmsg ("Attaching islands: ");
    last_progress = -1; 
    for (i = 1; i <= plus->n_isles; i++) {
        Vect_attach_isle ( Map, i ) ;

	/* print progress */
        progress = ( int ) 100 *  i / plus->n_isles;  
	if ( progress > last_progress ) {
            prnmsg ("%4d%%\b\b\b\b\b", progress);
	    last_progress = progress;
	}
    }
    
    /* Attach centroids to areas */
    prnmsg ("\nAttaching centroids: ");
    last_progress = -1; 
    for (area = 1; area <= plus->n_areas; area++) {
	/* print progress */
        progress = ( int ) 100 *  area / plus->n_areas;  
	if ( progress > last_progress ) {
            prnmsg ("%4d%%\b\b\b\b\b", progress);
	    last_progress = progress;
	}
	
	/* attach */
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
    }
    prnmsg ("\n");

    return 1;
}


