/****************************************************************************
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
#include "gis.h"
#include "Vect.h"

/*!
 \fn int dig_type_to_store (int type)
 \brief convert type to store type
 \return store type
 \param type
*/
int
dig_type_to_store (int type)
{
    switch ( type ) {
	case GV_POINT:
	   return GV_STORE_POINT;
	case GV_LINE:
	   return GV_STORE_LINE;
	case GV_BOUNDARY:
	   return GV_STORE_BOUNDARY;
	case GV_CENTROID:
	   return GV_STORE_CENTROID;
	case GV_AREA:
	   return GV_STORE_AREA;
	case GV_FACE:
	   return GV_STORE_FACE;
	case GV_KERNEL:
	   return GV_STORE_KERNEL;
	case GV_VOLUME:
	   return GV_STORE_VOLUME;
        default:
	   return 0;
    }
}

/*!
 \fn int Vect_type_from_store (int type)
 \brief convert type from store type
 \return store type
 \param type
*/
int
dig_type_from_store (int stype)
{
    switch ( stype ) {
	case GV_STORE_POINT:
	   return GV_POINT;
	case GV_STORE_LINE:
	   return GV_LINE;
	case GV_STORE_BOUNDARY:
	   return GV_BOUNDARY;
	case GV_STORE_CENTROID:
	   return GV_CENTROID;
	case GV_STORE_AREA:
	   return GV_AREA;
	case GV_STORE_FACE:
	   return GV_FACE;
	case GV_STORE_KERNEL:
	   return GV_KERNEL;
	case GV_STORE_VOLUME:
	   return GV_VOLUME;
        default:
	   return 0;
    }
}

