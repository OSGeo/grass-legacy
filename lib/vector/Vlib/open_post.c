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
#include "libpq-fe.h"

/* Open PostGIS vector.
*  Map->name and Map->mapset must be set before
*
*  Return: 0 success
*         -1 error
*/
int 
V1_open_old_post ( struct Map_info *Map)
{
    struct Format_info_post *info;
    char       query[1024];
    int        i;

    info = &(Map->fInfo.post);
#ifdef GDEBUG
    G_debug (1, "V1_open_old_post(): host = %s, database = %s",
                 Map->fInfo.post.host, Map->fInfo.post.database );
#endif

    /* make a connection to the specified database */
    Map->fInfo.post.conn = PQsetdbLogin(info->host, info->port, NULL, NULL, 
	    info->database, info->user, info->password);
     
    if ( PQstatus(info->conn) == CONNECTION_BAD ) {
        G_warning ("Connection to database '%s' failed:", info->database);
        G_warning ("%s", PQerrorMessage(info->conn));
        return (-1);
    }
     
    sprintf (query, "select %s from %s;", info->geom_geom, info->geom_table);
#ifdef GDEBUG
    G_debug (1, "query: %s", query );
#endif
    
    Map->fInfo.post.geomRes = PQexec(info->conn, query); 
    info->nGeom = PQntuples(Map->fInfo.post.geomRes);
    info->nextRow = 0;
    
    Map->head.with_z = WITHOUT_Z;

#ifdef GDEBUG
    G_debug (1, "V1_open_old_post(): %d geometry records read from DB", info->nGeom);
#endif
  
  return (0);
}

/* Open PostGIS vector.
*
*  Return: 0 success
*         -1 error
*/
int
V1_open_new_post (
    struct Map_info *Map,
    char *name,
    int with_z)
{
   Map->name = G_store (name);
   Map->mapset = G_store (G_mapset());
   Map->level = 1;
   Map->open = VECT_OPEN_CODE;
   return ( V1_open_old_post (Map) );
}

/* Open old file on level 2.
*  Map->name and Map->mapset must be set before
*
*  Return: 0 success
*         -1 error */
int
V2_open_old_post (struct Map_info *Map)
{
    return -1;
}

#endif
