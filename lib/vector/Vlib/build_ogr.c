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

#ifdef HAVE_OGR

extern FILE *Msgout;
extern int prnmsg ( char *msg, ...) ;

/*!
 \fn int Vect_build_ogr ( struct Map_info *Map, FILE *msgout ) 
 \brief build topology SHAPE
 \return 1 on success, 0 on error
 \param Map_info structure, msgout - message output (stdout/stderr for example) or NULL
*/
int
Vect_build_ogr ( struct Map_info *Map, FILE *msgout )
{
    struct Plus_head *plus ;
    
    plus = &(Map->plus);
    Msgout = msgout;

    Vect_rewind ( Map );
    
    G_warning ("Vect_build_ogr() not yet implemented" );
    return (0);
    
    return 1;
}

#endif
