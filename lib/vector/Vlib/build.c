#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "gis.h"
#include "Vect.h"

static FILE *Msgout = NULL;

int prnmsg ( char *msg, ...) {
    char buffer[1000]; 
    va_list ap; 
    
    if ( Msgout != NULL ) {
        va_start(ap,msg);
	vsprintf(buffer,msg,ap);
	va_end(ap);
	fprintf (Msgout, "%s\n", buffer);
    }
	
    return 1;	
}

/* Build topology 
*  msgout - message output (stdout/stderr for example) or NULL
*
*  Returns: 1 - success
*           0 - error
*/
int
Vect_build ( struct Map_info *Map, FILE *msgout ) {
    struct Plus_head *plus ;
    int    i, j, s, n, type, n_points, node, lineid, offset, ret;
    int    n_lines, side, line;
    int    area, isle, direction;
    int    found;
    plus_t *lines;
    struct line_pnts *Points, *APoints;
    struct line_cats *Cats;
    P_LINE_2D *Line, *BLine;
    P_NODE_2D *Node;
    P_AREA_2D *Area;
    P_ISLE_2D *Isle;
    double area_size, poly;
    FILE   *fp;
    char   fname[1024], buf[1024];
    
    plus = &(Map->plus);
    Msgout = msgout;
    
    prnmsg ("Building topology ...") ;
    dig_init_plus ( plus );
    
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
    prnmsg ("Registering lines ...");
    while ( 1 ) {
	offset = ftell (Map->dig_fp);
        type = Vect_read_next_line (Map, Points, Cats);
        if ( type == -1 ) {
	    fprintf (stderr, "\nERROR: vector file - can't read\n" );
	    return 0;
        } else if ( type == -2 ) {
	    break;
	}
	G_debug ( 3, "Register line: offset = %d", offset );
	lineid = dig_add_line ( plus, type, Points, offset );
    }
    prnmsg ("-> %d lines", plus->n_lines);

    /* Build areas */
    /* Go through all bundaries and try to build area for both sides */
    prnmsg ("Building areas ...");
    
    for (i = 1; i <= plus->n_lines; i++) {
	if ( plus->Line_2d[i] == NULL ) { continue; } /* dead line */
	Line = plus->Line_2d[i];
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
		BLine = plus->Line_2d[line];
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
    prnmsg ("-> %d areas, %d isles", plus->n_areas, plus->n_isles );

    /* Attache isles to areas */
    prnmsg ("Attaching islands ...");
    for (i = 1; i <= plus->n_isles; i++) {
        Isle = plus->Isle_2d[i];
        line = abs(Isle->lines[0]);
	Line = plus->Line_2d[line];
        node = Line->N1;
	Node = plus->Node_2d[line];
	
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
		
		Area = plus->Area_2d[j];
		n = Area->n_isles;
                if ( dig_area_alloc_isle_2d (Area, 1) == -1 )
                    return 0;
		
		Area->isles[n] = i;
		Area->n_isles++;
                G_debug ( 3, "n_isles = %d", Area->n_isles);
		break;
	    }
	}
    }
    
    /* Attache centroids to areas */
    prnmsg ("Attaching centroids ...");
    for (i = 1; i <= plus->n_lines; i++) {
	Line = plus->Line_2d[i];
	if ( Line->type != GV_CENTROID ) { continue; }
        
	Node = plus->Node_2d[Line->N1];
	found = 0;
        for (j = 1; j <= plus->n_areas; j++) {
	    ret = Vect_point_in_area (Map, j, Node->x, Node->y);
	    if ( ret ) {
                G_debug ( 3, "Centroid (line=%d) in area %d", i, j );
		Area = plus->Area_2d[j];
		n = Area->n_centroids;
                if ( dig_area_alloc_centroid_2d (Area, 1) == -1 )
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
    
    /*
    fprintf (stdout," Number of lines:   %d\n", Map.n_lines) ;
    fprintf (stdout," Number of nodes:   %d\n", Map.n_nodes) ;
    fprintf (stdout," Number of areas:   %d\n", Map.n_areas) ;
    fprintf (stdout," Number of isles:   %d\n", Map.n_isles) ;
    fprintf (stdout," Number of atts :   %d\n", Map.n_atts) ;

    fprintf (stdout," Number of unattached atts :   %d\n", tot_atts-Map.n_atts) ;
    fprintf (stdout," Snapped lines  :   %d\n", snapped) ;
    */

    /*  write out all the accumulated info to the plus file  */
    prnmsg ("Writting topo file ...");
    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
    G__file_name (fname, buf, GV_TOPO_ELEMENT, Map->mapset);
    G_debug (1, "Open topo: %s", fname);
    fp = fopen( fname, "w");
    if ( fp ==  NULL) {
        G_warning("Can't open topo file for write: %s\n", fname);
	    return 0;
    }

    /* set portable info */
    dig_init_portable ( &(plus->port), dig__byte_order_out ());
    
    /*
    Plus.all_areas = 1;
    if (do_islands)
	Plus.all_isles = 1;
    else
	Plus.all_isles = 0; 
    dig_map_to_head (&Map, &Plus);
    */
    /*  clean up files  */

    
    if ( 0 > dig_write_plus_file (fp, plus) ) {
        G_warning ("Error writing out topo file.\n");
	return 0;
    }
    
    fclose( fp );

    prnmsg ("Topology was built.") ;
    return 1;
}

/* Dump topology 
*  out - output (stdout/stderr for example)
*
*  Returns: 1 - success
*           0 - error
*/
int
Vect_topo_dump ( struct Plus_head *plus, FILE *out ) {
    int i, j, line, isle;
    P_NODE_2D *Node;
    P_LINE_2D *Line;
    P_AREA_2D *Area;
    P_ISLE_2D *Isle;

    fprintf (out, "---------- TOPOLOGY DUMP ----------\n" ); 
    
    /* nodes */
    fprintf (out, "Nodes (%d nodes):\n", plus->n_nodes ); 
    for (i = 1; i <= plus->n_nodes; i++) {
	if ( plus->Node_2d[i] == NULL ) { continue; }
	Node = plus->Node_2d[i];
	fprintf (out, "node = %d, n_lines = %d, xy = %f, %f\n", i, Node->n_lines,
	                            Node->x, Node->y ); 
        for (j = 0; j < Node->n_lines; j++) {
	    line = Node->lines[j];
	    Line = plus->Line_2d[abs(line)];
	    fprintf (out, "  line = %3d, type = %d, angle = %f\n", line, Line->type, Node->angles[j] ); 
	}
    }
    
    /* lines */
    fprintf (out, "Lines (%d lines):\n", plus->n_lines ); 
    for (i = 1; i <= plus->n_lines; i++) {
	if ( plus->Line_2d[i] == NULL ) { continue; }
	Line = plus->Line_2d[i];
	fprintf (out, "line = %d, type = %d, offset = %d n1 = %d, n2 = %d, left = %d, right = %d\n", 
		       i, Line->type, Line->offset, Line->N1, Line->N2,
	               Line->left, Line->right); 
    }
    
    /* areas */
    fprintf (out, "Areas (%d areas):\n", plus->n_areas ); 
    for (i = 1; i <= plus->n_areas; i++) {
	if ( plus->Area_2d[i] == NULL ) { continue; }
	Area = plus->Area_2d[i];
	fprintf (out, "area = %d, n_lines = %d, n_centroids = %d n_isles = %d\n", 
		 i, Area->n_lines, Area->n_centroids, Area->n_isles ); 
        for (j = 0; j < Area->n_lines; j++) {
	    line = Area->lines[j];
	    Line = plus->Line_2d[abs(line)];
	    fprintf (out, "  line = %3d\n", line ); 
	}
        for (j = 0; j < Area->n_centroids; j++) {
	    line = Area->centroids[j];
	    Line = plus->Line_2d[abs(line)];
	    fprintf (out, "  centroid = %3d\n", line ); 
	}
        for (j = 0; j < Area->n_isles; j++) {
	    isle = Area->isles[j];
	    fprintf (out, "  isle = %3d\n", isle ); 
	}
    }
    
    /* isles */
    fprintf (out, "Islands (%d islands):\n", plus->n_isles ); 
    for (i = 1; i <= plus->n_isles; i++) {
	if ( plus->Isle_2d[i] == NULL ) { continue; }
	Isle = plus->Isle_2d[i];
	fprintf (out, "isle = %d, n_lines = %d area = %d\n", i, Isle->n_lines, 
		           Isle->area ); 
        for (j = 0; j < Isle->n_lines; j++) {
	    line = Isle->lines[j];
	    Line = plus->Line_2d[abs(line)];
	    fprintf (out, "  line = %3d\n", line ); 
	}
    }

    return 1;
}

