/*
* $Id$
*
****************************************************************************
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

/*
*  Convert type to store type.
*
*  Returns  store type.
*/
int
Vect_type_to_store (int type)
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
        default:
	   return 0;
    }
}

/*
*  Convert type from store type.
*
*  Returns type.
*/
int
Vect_type_from_store (int stype)
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
        default:
	   return 0;
    }
}

/*
*  Get types from options.
*
*  Returns: types 
*           -1 error
*/
int
Vect_option_to_types (struct Option *type_opt )
{
    int i = 0;
    int type = 0; 

    while (type_opt->answers[i]) {
        switch ( type_opt->answers[i][0] ) {
	    case 'p':
	        type |= GV_POINT;
	        break;
            case 'l':
                type |= GV_LINE;
                break;
            case 'b':
	        type |= GV_BOUNDARY;
                break;
            case 'c':
                type |= GV_CENTROID;
                break;
            case 'a':
                type |= GV_AREA;
                break;
        }
        i++;
    }

    return type;
}

