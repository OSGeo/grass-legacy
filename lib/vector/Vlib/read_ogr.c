/****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Radim Blazek, Piero Cavalieri 
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
#include "gis.h"
#include "Vect.h"

/*
*  Recursively read feature and add all elements to points_cache and types_cache.
*  ftype : if > 0 use this type (because parts of Polygon are read as wkbLineString)
* 
*  return 0 OK
*         1 Error
*/
static int cache_feature ( struct Map_info *Map, OGRGeometryH hGeom, int ftype ) 
{
    int line, i, np, ng, tp;
    OGRwkbGeometryType type;
    OGRGeometryH hGeom2;

    G_debug (4, "cache_feature" ); 

    /* Alloc space */
    line = Map->fInfo.ogr.lines_num;
    if ( line == Map->fInfo.ogr.lines_alloc ) { 
	Map->fInfo.ogr.lines_alloc += 20;
	Map->fInfo.ogr.lines = (struct line_pnts **) G_realloc ( (void *)Map->fInfo.ogr.lines, 
		      Map->fInfo.ogr.lines_alloc * sizeof(struct line_pnts *) );

	Map->fInfo.ogr.lines_types = (int *) G_realloc ( Map->fInfo.ogr.lines_types, 
		      Map->fInfo.ogr.lines_alloc * sizeof(int) );

	for ( i = Map->fInfo.ogr.lines_num; i < Map->fInfo.ogr.lines_alloc; i++ ) 
	    Map->fInfo.ogr.lines[i] = Vect_new_line_struct();
	
    }
    Vect_reset_line ( Map->fInfo.ogr.lines[line] );
    
    type = wkbFlatten ( OGR_G_GetGeometryType (hGeom) );
    
    switch ( type ) {
	case wkbPoint:
	    G_debug (4, "Point" );
	    Vect_append_point ( Map->fInfo.ogr.lines[line], 
		                OGR_G_GetX(hGeom,0), OGR_G_GetY(hGeom,0), OGR_G_GetZ(hGeom,0) );
	    Map->fInfo.ogr.lines_types[line] = GV_POINT;
	    Map->fInfo.ogr.lines_num++;
	    return 0;
	    break;
	    
	case wkbLineString:
	    G_debug (4, "LineString" );
	    np = OGR_G_GetPointCount(hGeom);
	    for( i = 0; i < np; i++ ) {
                Vect_append_point ( Map->fInfo.ogr.lines[line],
			            OGR_G_GetX(hGeom,i), OGR_G_GetY(hGeom,i), OGR_G_GetZ(hGeom,i) );
            }
	    
	    if ( ftype > 0 ) { /* Polygon rings */
	        Map->fInfo.ogr.lines_types[line] = ftype;
	    } else { 
	        Map->fInfo.ogr.lines_types[line] = GV_LINE;
	    }
	    Map->fInfo.ogr.lines_num++;
	    return 0;
	    break;

	case wkbMultiPoint:
	case wkbMultiLineString:
	case wkbPolygon:
	case wkbMultiPolygon:
	case wkbGeometryCollection:
	    ng = OGR_G_GetGeometryCount( hGeom );
	    G_debug (4, "%d geoms -> next level", ng );
	    if ( type == wkbPolygon ) {
		tp = GV_BOUNDARY;
	    } else {
		tp = -1;
	    }
	    for ( i = 0 ; i < ng; i++ ) {
	        hGeom2 = OGR_G_GetGeometryRef( hGeom, i );
                cache_feature ( Map, hGeom2, tp );
	    }
	    return 0;
	    break;

	default:
	    G_warning ("OGR feature type %d not supported\n", type);
	    return 1;
	    break;
    }
}

/*
 *  Read next line from OGR layer. Skip empty features.
 *
 *  Returns  line type
 *           -2 No more features (EOF)
 *           -1 Out of memory
 *
 *  The action of this routine can be modified by:
 *    Vect_read_constraint_region ()
 *    Vect_read_constraint_type   ()
 *    Vect_remove_constraints     ()
 */
int
V1_read_next_line_ogr (struct Map_info *Map, struct line_pnts *line_p, struct line_cats *line_c)
{
    int      itype;
    BOUND_BOX lbox, mbox;
    OGRFeatureH hFeature;
    OGRGeometryH hGeom;

    G_debug (3, "V1_read_next_line_ogr()");

    if (line_p != NULL) Vect_reset_line (line_p);
    if (line_c != NULL) Vect_reset_cats (line_c);

    if (Map->Constraint_region_flag)
	Vect_get_constraint_box (Map, &mbox);

    while (1) {
	/* Read feature to chache if necessary */
	while ( Map->fInfo.ogr.lines_next == Map->fInfo.ogr.lines_num ) {
	        hFeature = OGR_L_GetNextFeature(Map->fInfo.ogr.layer);

		if ( hFeature == NULL ) { return -2; } /* no more features */

                hGeom = OGR_F_GetGeometryRef( hFeature );
                if ( hGeom == NULL ) { /* feature without geometry */ 
		    OGR_F_Destroy( hFeature );
		    continue; 
		}

		Map->fInfo.ogr.feature_cache_id = (int) OGR_F_GetFID ( hFeature );
		if ( Map->fInfo.ogr.feature_cache_id == OGRNullFID ) {
		    G_warning ("OGR feature without ID." );
		}

		/* Cache the feature */
		Map->fInfo.ogr.lines_num = 0; 
		cache_feature ( Map, hGeom, -1 );
		G_debug (4, "%d lines read to cache", Map->fInfo.ogr.lines_num);
		OGR_F_Destroy( hFeature );

		Map->fInfo.ogr.lines_next = 0; /* next to be read from cache */
	}

        /* Read next part of the feature */
        G_debug ( 4, "read next cached line %d", Map->fInfo.ogr.lines_next );
        itype = Map->fInfo.ogr.lines_types[Map->fInfo.ogr.lines_next]; 

	/* Constraint on Type of line 
	 * Default is all of  Point, Line, Area and whatever else comes along
	 */
	if (Map->Constraint_type_flag) {
	    if (!(itype & Map->Constraint_type)) {
		Map->fInfo.ogr.lines_next++;
		continue;
	    }
	}

	/* Constraint on specified region */
	if (Map->Constraint_region_flag) {
	    Vect_line_box ( Map->fInfo.ogr.lines[Map->fInfo.ogr.lines_next], &lbox);

	    if (!Vect_box_overlap (&lbox, &mbox)) {
		Map->fInfo.ogr.lines_next++;
		continue;
	    }
	}

        if (line_p != NULL) 
	    Vect_append_points ( line_p, Map->fInfo.ogr.lines[Map->fInfo.ogr.lines_next], GV_FORWARD ); 

        if (line_c != NULL && Map->fInfo.ogr.feature_cache_id != OGRNullFID ) 
	    Vect_cat_set ( line_c, 1, Map->fInfo.ogr.feature_cache_id );

	Map->fInfo.ogr.lines_next++;
        G_debug ( 4, "next line read, type = %d", itype );
	return (itype);
    }
    return -2; /* not reached */
}

/*
 *  Read next line from OGR layer.
 *
 *  Returns  line type
 *           -2 no more features (EOF)
 *           -1 Out of memory
 */
int
V2_read_next_line_ogr (struct Map_info *Map, struct line_pnts *line_p, struct line_cats *line_c)
{
    if ( Map->next_line >= Map->plus.n_lines ) return -2;

    return V2_read_line_ogr (Map, line_p, line_c, Map->next_line++);
}

/* Recursively descend to feature and read the part
 * return: 0 - OK
 *         1 - error
 */
static int read_line ( struct Map_info *Map, OGRGeometryH hGeom, int offset, struct line_pnts *Points )
{
    int     i, nPoints;
    int     eType;
    OGRGeometryH hGeom2;

    /* Read coors if hGeom is a simple element (wkbPoint, wkbLineString) otherwise
     * descend to geometry specified by offset[offset] */

    G_debug (4, "read_line() offset = %d", offset);

    eType = wkbFlatten (OGR_G_GetGeometryType (hGeom));
    G_debug (4, "OGR Geometry of type: %d", eType);

    switch ( eType ) {
	case wkbPoint:
	    G_debug (4, "Point" );
	    Vect_append_point ( Points, OGR_G_GetX(hGeom,0), OGR_G_GetY(hGeom,0), OGR_G_GetZ(hGeom,0) );
	    return 0;
	    break;

	case wkbLineString:
	    G_debug (4, "LineString" );
	    nPoints = OGR_G_GetPointCount(hGeom);
	    for( i = 0; i < nPoints; i++ ) {
		Vect_append_point ( Points, OGR_G_GetX(hGeom,i), OGR_G_GetY(hGeom,i), OGR_G_GetZ(hGeom,i) );
	    }
	    return 0;
	    break;

	case wkbPolygon:
	case wkbMultiPoint:
	case wkbMultiLineString:
	case wkbMultiPolygon:
	case wkbGeometryCollection:
	    G_debug (4, " more geoms -> part %d", Map->fInfo.ogr.offset[offset] );
	    hGeom2 = OGR_G_GetGeometryRef( hGeom, Map->fInfo.ogr.offset[offset] );
            return ( read_line ( Map, hGeom2, offset+1, Points ) );
	    break;

	default:
	    G_warning ("OGR feature type %d not supported\n", eType);
	    break;
    }
    return 1;
}

/*
 *  Read line from layer on given offset.
 *
 *  Returns  line type
 *           -2 End of table (last row)
 *           -1 Out of memory
 */
int
V2_read_line_ogr (struct Map_info *Map, struct line_pnts *line_p,  struct line_cats *line_c, int line)
{
    int      node;
    int     offset, FID;
    OGRGeometryH hGeom;
    P_LINE *Line;
    P_NODE  *Node;

    G_debug (4, "V2_read_line_ogr() line = %d", line);

    if (line_p != NULL) Vect_reset_line (line_p);
    if (line_c != NULL) Vect_reset_cats (line_c);
    
    Line = Map->plus.Line[line];
    offset = (int) Line->offset;

    if ( Line->type == GV_CENTROID ) {
	G_debug (4, "Centroid");
	node = Line->N1;
	Node = Map->plus.Node[node];

	/* coordinates */
	if (line_p != NULL) {
	    Vect_append_point (line_p, Node->x, Node->y, 0.0);
	}

	/* category */
	if (line_c != NULL) {
	    /* cat = FID+1 and offset = FID for centroid */
	    Vect_cat_set (line_c, 1, (int) offset + 1); 
	}

	return (GV_CENTROID);
    } else {
	FID = Map->fInfo.ogr.offset[offset];
        G_debug (4, "  FID = %d", FID);
	
	/* coordinates */
	if (line_p != NULL) {
	    /* Read feature to cache if necessary */
	    if ( Map->fInfo.ogr.feature_cache_id != FID ) {
		G_debug (4, "Read feature (FID = %d) to cache.", FID);
		if ( Map->fInfo.ogr.feature_cache ) {
		    OGR_F_Destroy( Map->fInfo.ogr.feature_cache );
		}
		Map->fInfo.ogr.feature_cache = OGR_L_GetFeature ( Map->fInfo.ogr.layer, FID );
		if ( Map->fInfo.ogr.feature_cache == NULL ) {
		    G_fatal_error ( "Cannot read feature, FID = %d", FID );
		}
	    }
		
	    hGeom =  OGR_F_GetGeometryRef ( Map->fInfo.ogr.feature_cache );
	    if (hGeom == NULL) {
		G_fatal_error ( "Cannot get feature geometry, FID = %d", FID );
	    }
	    
	    read_line ( Map, hGeom, Line->offset + 1, line_p );
	}
	    
	/* category */
	if (line_c != NULL) {
	    Vect_cat_set (line_c, 1, FID + 1);
	}

	return Line->type;
    }
    
    return -2; /* not reached */
}

