/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes or Mike Higgins.
*               Update to GRASS 5.7 Radim Blazek and David D. Gray.
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
#include <unistd.h>
#include <string.h>
#include "Vect.h"
#include "gis.h"
#include "shapefil.h"

#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_OGR

/* Open old file.
*  Map->name and Map->mapset must be set before
*
*  Return: 0 success
*         -1 error
*/
int 
V1_open_old_ogr ( struct Map_info *Map, int update )
{
    G_debug ( 1, "V1_open_old_ogr(): dsn = %s layer = %s", Map->fInfo.ogr.dsn, Map->fInfo.ogr.layer_name );

    if ( update ) {
        G_warning ( "OGR format cannot be updated.");
        return -1;
    }

    if ( Map->fInfo.ogr.dsn == NULL || Map->fInfo.ogr.layer_name == NULL ) {
	G_warning ("OGR DSN or layer name not defined\n");
	return (-1);
    }

    G_warning ("V1_open_old_ogr() not yet implemented" );
    return (-1);
    
    Map->head.with_z = WITHOUT_Z;

    return (0);
}

/* Open new file.
*
*  Return: 0 success
*         -1 error
*/
int
V1_open_new_ogr (
    struct Map_info *Map,
    char *name,
    int with_z)
{
    G_warning ( "OGR format cannot be created." );
    return (-1);
}

#endif
