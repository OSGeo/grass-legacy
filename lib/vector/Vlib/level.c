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
#include "Vect.h"


/*
   ** Simply returns level that Map is opened at
   **  returns open level or -1 on error
 */
int
Vect_level (Map)
     struct Map_info *Map;
{
  if (Map->open != VECT_OPEN_CODE)
    {
      if (Map->open != VECT_CLOSED_CODE)
	fprintf (stderr, "VECT_LEVEL: Map structure was never initialized\n");
      else
	fprintf (stderr, "VECT_LEVEL: Map structure has been closed\n");
      return (-1);
    }
  return (Map->level);
}
