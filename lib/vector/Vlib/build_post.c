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
#include "gis.h"
#include "Vect.h"

/*!
 \fn int Vect_build_post ( struct Map_info *Map, FILE *msgout )
 \brief build topology PostGRASS
 \return 1 on success, 0 on error
 \param Map_info structure, msgout - message output (stdout/stderr for example) or NULL
*/
int
Vect_build_post ( struct Map_info *Map, FILE *msgout )
{

    G_debug (1, "Vect_build_post()"); 

    return ( Vect_build_nat (Map, msgout)) ;
}


