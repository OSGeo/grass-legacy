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
 \fn int Vect_option_to_types (struct Option *type_opt )
 \brief get types from options
 \return types, -1 on error
 \param Option structure
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
            case 'f':
                type |= GV_FACE;
                break;
            case 'k':
                type |= GV_KERNEL;
                break;
            case 'a':
                type |= GV_AREA;
                break;
            case 'v':
                type |= GV_VOLUME;
                break;
        }
        i++;
    }

    return type;
}

