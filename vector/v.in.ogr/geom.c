/* ***************************************************************
 * *
 * * MODULE:       v.in.ogr
 * * 
 * * AUTHOR(S):    Radim Blazek
 * *               
 * * PURPOSE:      Import OGR vectors
 * *               
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * **************************************************************/
#include <stdlib.h> 
#include <string.h> 
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "ogr_api.h"

/* Write geometry to output map */
int 
geom(OGRGeometryH hGeom, struct Map_info *Map, int cat )
{
    int    i, j, np, nr, ret;
    struct line_pnts *Points, **IPoints;
    struct line_cats *BCats, *Cats ;
    OGRwkbGeometryType eType;
    OGRGeometryH hRing;
    double  x, y;

    Points = Vect_new_line_struct ();
    BCats = Vect_new_cats_struct ();
    Cats = Vect_new_cats_struct ();
    Vect_cat_set ( Cats, 1, cat );

    eType = wkbFlatten(OGR_G_GetGeometryType(hGeom));

    if( eType == wkbPoint ) {
         Vect_append_point ( Points, OGR_G_GetX(hGeom,0), OGR_G_GetY(hGeom,0), OGR_G_GetZ(hGeom,0) );
         Vect_write_line ( Map, GV_POINT, Points, Cats);
    }
    else if( eType == wkbLineString )
    {
        np = OGR_G_GetPointCount(hGeom);

        for( i = 0; i < np; i++ ) {
            Vect_append_point ( Points, OGR_G_GetX(hGeom,i), 
		    OGR_G_GetY(hGeom,i), OGR_G_GetZ(hGeom,i) );
	}
        Vect_write_line ( Map, GV_LINE, Points, Cats);
    }

    else if( eType == wkbPolygon )
    {
        nr = OGR_G_GetGeometryCount( hGeom );

	/* SFS: 1 exterior boundary and 0 or more interior boundaries.
	*  So I hope that exterior is the first one, even if it is not explicitly told  */

	/* Area */
	hRing = OGR_G_GetGeometryRef( hGeom, 0 );
	np = OGR_G_GetPointCount(hRing);
	Vect_reset_line ( Points );
	for( j = 0; j < np; j++ ) {
	    Vect_append_point ( Points, OGR_G_GetX(hRing,j), 
		    OGR_G_GetY(hRing,j), OGR_G_GetZ(hRing,j) );
	}
        Vect_write_line ( Map, GV_BOUNDARY, Points, BCats);
	
	/* Isles */
	IPoints = (struct line_pnts **) G_malloc ( (nr-1) * sizeof (struct line_pnts *) );
        for( i = 1; i < nr; i++ ) {
	    IPoints[i-1] = Vect_new_line_struct ();
            hRing = OGR_G_GetGeometryRef( hGeom, i );
            np = OGR_G_GetPointCount(hRing);

            for( j = 0; j < np; j++ ) {
                Vect_append_point ( IPoints[i-1], OGR_G_GetX(hRing,j), 
			OGR_G_GetY(hRing,j), OGR_G_GetZ(hRing,j) );
	    }
            Vect_write_line ( Map, GV_BOUNDARY, IPoints[i-1], BCats);
        }

	/* Centroid */
	ret = Vect_get_point_in_poly_isl ( Points, IPoints, nr-1, &x, &y);
	if ( ret == -1 ) {
	    G_warning ( "Cannot calculate centroid" );
	} else {
            Vect_reset_line ( Points );
	    Vect_append_point ( Points, x, y, 0 );
	    Vect_write_line ( Map, GV_CENTROID, Points, Cats);
	}
	for( i = 1; i < nr; i++ ) {
            Vect_destroy_line_struct ( IPoints[i-1] );
	}
        if ( nr > 1 ) G_free ( IPoints );	 
     	
    }

    /* I did not test this because I did not have files of these types */
    else if( eType == wkbGeometryCollection
             || eType == wkbMultiPolygon
             || eType == wkbMultiLineString
             || eType == wkbMultiPoint )
    {
	G_debug (3, "GeometryCollection or MultiPolygon/LineString/Point" );
        nr = OGR_G_GetGeometryCount( hGeom );
        for( i = 0; i < nr; i++ ) {
            hRing = OGR_G_GetGeometryRef( hGeom, i );

	    ret = geom( hRing, Map, cat );
	    if ( ret == -1 ) {
		G_warning ("Cannot write part of geometry" );
	    }
        }
    }
   
    else { 
	G_fatal_error ("Unknown geometry type");
    }

    Vect_destroy_line_struct ( Points );
    Vect_destroy_cats_struct ( Cats );
    Vect_destroy_cats_struct ( BCats );
    
    return 0;
}


