/****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Radim Blazek
*
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
#include <stdio.h>
#include <string.h>
#include "Vect.h"

/*!
 \fn int Vect_hist_command ( struct Map_info *Map )
 \brief write command info to history file
 \return 0 OK, -1 error
 \param struct Map_info *Map
*/
int 
Vect_hist_command ( struct Map_info *Map )
{
    char *cmd, buf[2000];

    G_debug (3, "Vect_hist_command()");

    cmd = G_recreate_command();

    Vect_hist_write ( Map, "COMMAND: " );
    Vect_hist_write ( Map, cmd );
    Vect_hist_write ( Map, "\n" );

    sprintf ( buf, "GISDBASE: %s\n", G_gisdbase()); /* Needed ?*/
    Vect_hist_write ( Map, buf );

    sprintf ( buf, "LOCATION: %s MAPSET: %s USER: %s DATE: %s\n", 
	            G_location(), G_mapset(), G_whoami(), G_date()); /* Needed ?*/
    Vect_hist_write ( Map, buf );
    
    return 0;
}

/*!
 \fn int Vect_hist_write ( struct Map_info *Map, char *str )
 \brief write string to history file
 \return the number of characters printed
 \param struct Map_info *Map, char *str
*/
int 
Vect_hist_write ( struct Map_info *Map, char *str )
{
    int ret ;
    
    G_debug (5, "Vect_hist_write()");
    ret = fprintf ( Map->hist_fp, str ); 
    fflush ( Map->hist_fp );

    return ( ret ); 
}

/*!
 \fn char Vect_hist_read ( char *s, int size, struct Map_info *Map )
 \brief reads one line from history file without newline character
 \return return s on success, and NULL on error or EOF
 \param s buffer, allocated space must be size+1
 \param size maximum number of character
 \param Map 
*/
char * 
Vect_hist_read ( char *s, int size, struct Map_info *Map )
{
    int ret;
    G_debug (5, "Vect_hist_read()");

    if ( Map->hist_fp == NULL ) return NULL; /* OK for shapefile etc. */

    ret = G_getl2 (s, size, Map->hist_fp);

    if ( ret == 1 ) return s;

    return NULL;
}

/*!
 \fn int Vect_hist_rewind ( struct Map_info *Map )
 \brief rewind history file
 \return the number of characters printed
 \param struct Map_info *Map, char *str
*/
void 
Vect_hist_rewind ( struct Map_info *Map )
{
    G_debug (3, "Vect_hist_rewind()");

    if ( Map->hist_fp != NULL )
        rewind ( Map->hist_fp );
}

/*!
 \fn int Vect_hist_copy ( struct Map_info *In, struct Map_info *Out )
 \brief copy history from one map to another
 \return 0 OK, -1 error
 \param struct Map_info *In, struct Map_info *Out
*/
int 
Vect_hist_copy ( struct Map_info *In, struct Map_info *Out )
{
    int red, ret;
    char buf[1000];
    
    G_debug (3, "Vect_hist_copy()");

    if ( In->hist_fp == NULL ) return 0; /* This is correct (old hist doesn't exist) */
    if ( Out->hist_fp == NULL ) return -1; 

    fseek ( Out->hist_fp, 0, SEEK_END);
    rewind ( In->hist_fp );

    while ( (red = fread (buf, 1, 1000, In->hist_fp)) ) {
        if ( !(ret = fwrite (buf, 1, red, Out->hist_fp))) {
	    return (-1);
        }
	fflush ( Out->hist_fp );
    }

    /* In ends with \n ? */
    fseek ( In->hist_fp, -1, SEEK_END);
    if ( fread ( buf, 1, 1, In->hist_fp) != 1 ) {
        return -1;
    }

    if ( buf[0] != '\n' ) {
        Vect_hist_write ( Out, "\n");
    }

    /* Separator */
    Vect_hist_write ( Out, "---------------------------------------------------------------------------------\n");
    return ( 0 ); 
}

