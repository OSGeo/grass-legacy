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
#include "gis.h"
#include "Vect.h"

#ifdef HAVE_POSTGRES

/* Write line to database.
* 
*  Returns: record id
*           -1 for error */
long 
V1_write_line_post (
		     struct Map_info *Map,
		     int type,
		     struct line_pnts *points,
		     struct line_cats *cats)
{
    int  i;
    long id;
    char buf[2000];
    struct Format_info_post *info;
    PGresult   *res;

#ifdef GDEBUG
    G_debug (3, "V1_write_line_post()" );
#endif 
   
    info = &(Map->fInfo.post);
    
    sprintf(buf, "insert into %s (%s) values (GeometryFromText('LINESTRING (",
                        info->geom_table, info->geom_geom); 

    for ( i = 0; i < points->n_points; i++ ) {
	if ( i > 0) sprintf(buf, "%s, ", buf); 
	
        sprintf(buf, "%s %f %f", buf,  points->x[i], points->y[i]); 
    } 
    sprintf(buf, "%s)', -1) );", buf); 
	
#ifdef GDEBUG
       G_debug (3, "%s", buf );
#endif 

    res = PQexec(info->conn, buf); 
       
    return (1);
}

/* Rewrites line at the given id.
* 
*  Returns: record id
*           -1 for error */
long 
V1_rewrite_line_post (
		       struct Map_info *Map,
		       long id,
		       int type,
		       struct line_pnts *points,
		       struct line_cats *cats)
{
  int  n_points;
  FILE *dig_fp;
 
  G_warning ( "V1_rewrite_line_post() is not implemented");

  /* TODO */ 

  return (-1);
}

#endif

