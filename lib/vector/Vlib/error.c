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
#include "gis.h"
#include "Vect.h"

static int fatal_err = GV_FATAL_EXIT;

/*!
 \fn int Vect_set_fatal_error (int err)
 \brief set behaviour if fatal error occurs in some functions
 \return 0 on success
 \param    GV_FATAL_EXIT(default): print error message and exit,
   GV_FATAL_PRINT: print error message and return error,
   GV_FATAL_RETURN: return error
*/
int 
Vect_set_fatal_error (int err)
{
    fatal_err = err;
    return 0;
}

/*!
 \fn int Vect_get_fatal_error (void)
 \brief get behaviour for fatal error
 \return   GV_FATAL_EXIT(default): print error message and exit,
   GV_FATAL_PRINT: print error message and return error,
   GV_FATAL_RETURN: return error
 \param void
*/
int 
Vect_get_fatal_error ()
{
    return (fatal_err);
}



