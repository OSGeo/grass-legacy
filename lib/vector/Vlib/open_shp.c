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
#include <unistd.h>
#include <string.h>
#include "Vect.h"
#include "gis.h"
#include "shapefil.h"

#include <sys/types.h>
#include <sys/stat.h>

/* Open old file.
*  Map->name and Map->mapset must be set before
*
*  Return: 0 success
*         -1 error
*/
int 
V1_open_old_shp ( struct Map_info *Map, int update )
{
    SHPHandle hShp;
    DBFHandle hDbf;
    DBFFieldType col_type;
    int       ShapeType, nShapes;
    double    MinBound[4], MaxBound[4];
    int       i, j;
    char      col_name[15];

    G_debug ( 1, "V1_open_old_shp(): shp file = %s", Map->fInfo.shp.file );

    if ( update ) {
        G_warning ( "Shapefile format cannot be updated.");
        return -1;
    }

    if ( Map->fInfo.shp.file == NULL ) {
	G_warning ("Shapefile name not defined\n");
	return (-1);
    }

    hShp = SHPOpen( Map->fInfo.shp.file, "r" );
    if ( hShp == NULL) {
	G_warning ("Cannot open shapefile: %s", Map->fInfo.shp.file);
	return (-1);
    }

    hDbf = DBFOpen( Map->fInfo.shp.file, "r" );
    if ( hDbf == NULL) {
	G_warning ("Cannot open dbf file: %s", Map->fInfo.shp.file);
	SHPClose( hShp );
	return (-1);
    }
   
    /* find category column */
    Map->fInfo.shp.cat_col_num = -1;
    if ( Map->fInfo.shp.cat_col != NULL ) {
	for( i = 0; i < DBFGetFieldCount(hDbf); i++ ) {
	    col_type = DBFGetFieldInfo( hDbf, i, col_name, NULL, NULL );
	    
	    if( strcasecmp( Map->fInfo.shp.cat_col, col_name) == 0 ) {
		if ( col_type == FTInteger ) {
		    Map->fInfo.shp.cat_col_num = i;
		}
		break;
	    }
	}
	if ( Map->fInfo.shp.cat_col_num == -1 ) { /* print error message */
	    G_warning ("Column '%s' not found or is not integer. Available columns:", 
		             Map->fInfo.shp.cat_col);
	    for( j = 0; j < DBFGetFieldCount(hDbf); j++ ) {
		col_type = DBFGetFieldInfo( hDbf, j, col_name, NULL, NULL );
		fprintf ( stderr, "%s ", col_name);
		if ( col_type == FTInteger ) 
		    fprintf ( stderr, "(Integer)\n");
		else
		    fprintf ( stderr, "(Non-Integer)\n");
	    }
	}
    }
    G_debug ( 1, "category column number = %d", Map->fInfo.shp.cat_col_num );
    
    Map->fInfo.shp.hShp = hShp;
    Map->fInfo.shp.hDbf = hDbf;
    Map->fInfo.shp.shape = 0;
    Map->fInfo.shp.part = 0;

    SHPGetInfo( hShp, &nShapes, &ShapeType, MinBound, MaxBound );
    Map->fInfo.shp.type = ShapeType;
    Map->fInfo.shp.nShapes = nShapes;

    G_debug ( 0, "V1_open_old_shp(): shptype = %d, nShapes = %d", ShapeType, nShapes );
    
    Map->head.with_z = WITHOUT_Z;

  return (0);
}

/* Open old file.
*  Map->name and Map->mapset must be set before
*
*  Return: 0 success
*         -1 error
*/
int 
V2_open_old_shp ( struct Map_info *Map, int update )
{
    int ret;    
    char buf[500];
    FILE *fp;
    struct Coor_info CInfo;
 
    if ( update ) {
        G_warning ( "Shapefile format cannot be updated.");
        return -1;
    }

    /* check if topo is available */
    Vect_coor_info ( Map, &CInfo);
    
    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
    fp = G_fopen_old (buf, GV_TOPO_ELEMENT, Map->mapset);

    if ( fp == NULL ) { /* topo file is not available */
        G_debug( 1, "Cannot open topo file for vector '%s@%s'.\n",
	  	      Map->name, Map->mapset);
	return -1;
    }
    
    ret = V1_open_old_shp ( Map, 0 );
    if ( ret != 0 ) {
	fclose ( fp );
        return -1;
    }

    /* load topo to memory */
    dig_init_plus ( &(Map->plus) );
    dig_load_plus ( &(Map->plus), fp );

    fclose ( fp ); 
	
    return 0;
}

/* Open new file.
*
*  Return: 0 success
*         -1 error
*/
int
V1_open_new_shp (
    struct Map_info *Map,
    char *name,
    int with_z)
{
    G_warning ( "V1_open_new_shp() is not implemented." );
    return (-1);
}

