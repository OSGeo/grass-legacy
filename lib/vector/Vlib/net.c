/*
* $Id$
*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Radim Blazek
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
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"

static int clipper ( gnGrpGraph_s    *pgraph ,
                     gnGrpSPClipInput_s  * pargIn ,
                     gnGrpSPClipOutput_s * pargOut ,
                     void *          pvarg )         /* caller's pointer */
{
    double cost;
    
    G_debug ( 2, "Net: clipper()" );

    if ( gnGrpGet_NodeAttrSize(pgraph) > 0 ) {
	memcpy( &cost, GNGRP_NODE_ATTR_PTR(pargIn->pnNodeFrom), sizeof(cost) );
	G_debug ( 2, "  node = %d pcost = %d + %f (arc + node)", 
		           GNGRP_NODE_ID(pargIn->pnNodeFrom), pargOut->nLinkCost, cost );
	pargOut->nLinkCost += (gnInt32_t) cost;
    }
    return 0;   
}																															
/* Build network graph 
*
*  Returns: 0 - success
*           1 - error
*/
int
Vect_net_build_graph (  struct Map_info *Map,
			int ltype,   /* line type for arcs */
       			int afield,  /* arc costs field (if 0, use length) */
			int nfield,  /* node costs field (if 0, do not use node costs) */
			char *afcol, /* column with forward costs for arc */
			char *abcol, /* column with backward costs for arc (if NULL, back = forward) */
			char *ncol,  /* column with costs for nodes */
			int geo,     /* use geodesic calculation for length (LL) */
		        int algorithm ) /* not used, in future code for algorithm */
{
    int    i, j, from, to, line, nlines, ret, type, cat, skipped, cfound;
    int    *vals, nval, dofw, dobw;
    struct line_pnts *Points;
    struct line_cats *Cats;
    double dcost, ll;
    gnInt32_t  cost, bcost;
    gnGrpGraph_s *gr;
    gnInt32_t opaqueset[ 16 ] = { 360000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    struct    field_info *Fi;
    dbDriver  *driver;
    dbHandle  handle;
    dbString  stmt;
    char      buf[1000];

    /* TODO int costs -> double */
    G_debug (1, "Vect_build_graph(): ltype = %d, afield = %d, nfield = %d", ltype, afield, nfield); 
    G_debug (1, "    afcol = %s, abcol = %s, ncol = %s", afcol, abcol, ncol); 

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();

    ll = 0;
    if( G_projection() == 3) /* LL */
        ll = 1;
	
    gr = &(Map->graph);
    if ( nfield > 0 )
        gnGrpInitialize ( gr, 1, sizeof(double), 0, opaqueset ); 
    else
        gnGrpInitialize ( gr, 1, 0, 0, opaqueset ); 

    if ( gr == NULL )
        G_fatal_error ("Cannot build network graph"); 
    
    db_init_handle (&handle);
    db_init_string ( &stmt);
    
    /* --- Add arcs --- */
    /* Open db connection */
    if ( afield > 0 ) {
        Fi = Vect_get_field_info( Map->name, Map->mapset, afield);
	if ( Fi == NULL ) 
	    G_fatal_error ("Cannot get field info");
	
	driver = db_start_driver(Fi->driver);
	db_set_handle (&handle, Fi->database, NULL);
        
	if (db_open_database(driver, &handle) != DB_OK)
            G_fatal_error("Cannot open database %s", Fi->database) ;
    }
	
    skipped = 0;
    nlines = Vect_get_num_lines ( Map );
    G_debug ( 2, "Register arcs");
    fprintf ( stderr, "Registering arcs: ");
    for ( i = 1; i <= nlines; i++ ) {
	dofw = dobw = 1;
	Vect_get_line_nodes ( Map, i, &from, &to );
	type = Vect_read_line ( Map, Points, Cats, i );
        if ( !(type & (GV_LINE | GV_BOUNDARY) ) ) continue;
	
	if ( afield > 0 ) {
	    if ( !(Vect_cat_get(Cats, afield, &cat) ) ) {
		G_debug ( 2, "Category of field %d not attached to the line %d -> line skipped", afield, i);
		skipped += 2; /* Both directions */ 
		continue;
	    } else {
                sprintf( buf, "%s = %d", Fi->key, cat);
		G_debug ( 2, "WHERE: %s", buf );
		nval = db_select_int ( driver, Fi->table, afcol, buf, &vals);
		if ( nval < 1 ) {
		    G_warning ( "Database record for line %d (cat = %d, forward direction) not found " 
			        "(direction of line skipped)", i, cat);
		    dofw = 0;
		} else { 
		    cost = vals[0];
		    G_debug ( 2, "line = %d cat = %d cost = %d", i, cat, cost );
		}
		if ( abcol != NULL ) {
		    nval = db_select_int ( driver, Fi->table, abcol, buf, &vals);
		    if ( nval < 1 ) {
			dobw = 0;
		    } else { 
			bcost = vals[0];
		    }
		} else {
		    if (dofw)
		        bcost = cost;
		    else
			dobw = 0;
		}
		if ( dobw )
	            G_debug ( 2, "line = %d cat = %d bcost = %d", i, cat, bcost );
		else
		    G_warning ( "Database record for line %d (cat = %d, backword direction) not found", 
			        "(direction of line skipped)", i, cat);
	    }
	} else {
	    if ( ll ) { 
		if ( geo )
	            cost = (gnInt32_t) Vect_line_geodesic_length ( Points );
		else
	            cost = (gnInt32_t) 1000000 * Vect_line_length ( Points );
	    } else
	        cost = (gnInt32_t) Vect_line_length ( Points );
	    
	    bcost = cost;
	}
	if ( dofw ) {
            G_debug (3, "Add arc %d from %d to %d cost = %d", i, from, to, cost); 
	    ret = gnGrpAddLink ( gr, from, to, cost, i, NULL, NULL, NULL);
            if ( ret < 0 )
                G_fatal_error ("Cannot add network arc");
	}
	
	if ( dobw ) {
            G_debug (3, "Add arc %d from %d to %d bcost = %d", -i, to, from, bcost); 
	    ret = gnGrpAddLink ( gr, to, from, bcost, -i, NULL, NULL, NULL);
            if ( ret < 0 )
                G_fatal_error ("Cannot add network arc");
        }
	    
	G_percent ( i, nlines, 1 );
    }
    G_debug ( 2, "Arcs registered");
    
    if ( afield > 0 && skipped > 0 ) 
        G_debug ( 2, "%d lines missing category of field %d skipped", skipped, afield);
    
    if ( afield > 0 ) {
        db_close_database(driver);
	db_shutdown_driver(driver);
    }

    /* Set node attributes */
    if ( nfield > 0 ) {
        G_debug ( 2, "Set nodes' costs");
        Fi = Vect_get_field_info( Map->name, Map->mapset, nfield);
	if ( Fi == NULL ) 
	    G_fatal_error ("Cannot get field info");
	
	driver = db_start_driver(Fi->driver);
	db_set_handle (&handle, Fi->database, NULL);
        
	if (db_open_database(driver, &handle) != DB_OK)
            G_fatal_error("Cannot open database %s", Fi->database) ;
	
	for ( i = 1; i <= Vect_get_num_nodes ( Map ); i++ ) {
	    /* TODO: what happens if we set attributes of not existing node (skipped lines,
	     *       nodes without lines) */

	    nlines = Vect_get_node_n_lines ( Map, i );
	    G_debug ( 2, "  node = %d nlines = %d", i, nlines );
	    cfound = 0;
	    cost = 0;
	    for ( j = 0; j < nlines; j++ ) {
	        line = Vect_get_node_line ( Map, i, j );
	        G_debug ( 2, "  line (%d) = %d", j, line );
		type = Vect_read_line ( Map, NULL, Cats, line);
		if ( !(type & GV_POINT) ) continue;
	        if ( Vect_cat_get(Cats, nfield, &cat) ) { /* point with category of field found */
		    /* Read costs from database */
		    sprintf( buf, "%s = %d", Fi->key, cat);
		    G_debug ( 2, "node: WHERE: %s", buf );
		    nval = db_select_int ( driver, Fi->table, ncol, buf, &vals);
		    if ( nval < 1 ) {
			G_warning ( "Database record for node %d (cat = %d) not found " 
				    "(cost set to 0)", i, cat);
		    } else { 
			cost = vals[0];
			G_debug ( 2, "node = %d cat = %d cost = %d", i, cat, cost );
		    }
		    
		    cfound = 1;
		    break;
		}
	    }
	    if ( !cfound ) {
		G_debug ( 2, "Category of field %d not attached to any points in node %d"
			     "(costs set to 0)", nfield, i);
	    }
	    G_debug ( 2, "Set node's cost to %d", cost);
	    dcost = cost;
	    gnGrpSetNodeAttr  ( gr, &dcost, i); 
	}
        G_debug ( 2, "Nodes' costs were set");
    }
    
    if ( nfield > 0 ) {
        db_close_database(driver);
	db_shutdown_driver(driver);
    }

    G_debug (1, "Flattening the graph ..."); 
    ret = gnGrpFlatten ( gr );
    if ( ret < 0 )  
        G_fatal_error ("GngFlatten error");

    G_debug (1, "Graph was build."); 

    return 0;
}


/* Find shortest path.
*
*  List (must be initialised before) is filled with arcs (lines).
*
*  Returns: number of segments : ( 0 is correct for from = to )
*              ? sum of costs is better return value
*           -1 : destination unreachable
*/
int
Vect_net_shortest_path ( struct Map_info *Map, int from, int to, struct ilist *List, double *cost ) 
{
    int i, line, *pclip, cArc;
    gnGrpSPReport_s * pSPReport;

    Vect_reset_list ( List);
    
    pclip = NULL;
    if ( (pSPReport =  gnGrpShortestPath ( &(Map->graph), from, to, clipper, pclip )) == NULL ) {
        if (  gnGrpErrno( &(Map->graph) ) == 0  ) {
            printf( "Destination node %d is unreachable from node %d\n\n" , to , from );
	    return -1;
	}
        else
                 fprintf( stderr , "gnGrpShortestPath error: %s\n", gnGrpStrerror( &(Map->graph) ) );
    }
    for( i = 0 ; i < pSPReport->cArc ; i ++ ) {
	line = GNGRP_LINK_USER(pSPReport->pArc[i].Link);
        G_debug( 2, "From %ld to %ld - cost %ld user %d distance %ld\n" ,
                      GNGRP_NODE_ID(pSPReport->pArc[i].From),
                      GNGRP_NODE_ID(pSPReport->pArc[i].To),
                      GNGRP_LINK_COST(pSPReport->pArc[i].Link), /* this is the cost from clip() */
                      line,
                      pSPReport->pArc[i].Distance );
        Vect_list_append ( List, line );
    }

    cArc = pSPReport->cArc;
    gnGrpFreeSPReport( &(Map->graph), pSPReport );
	
    if ( cost != NULL )
	*cost = (double) pSPReport->distance;

    return (cArc);
}

