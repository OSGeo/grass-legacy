/*
* $Id$
*
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
#include <stdlib.h>
#include "gis.h"


struct ilist *
Vect_new_list (void)
{
  struct ilist *p;

  p = (struct ilist *) malloc (sizeof (struct ilist));

  if (p) {
    p->value = NULL;
    p->n_values = 0;
    p->alloc_values = 0;
  }
  
  return p;
}

int 
Vect_reset_list (struct ilist *list)
{
  list->n_values = 0;

  return 0;
}

int 
Vect_destroy_list (struct ilist *list)
{
  if (list)			/* probably a moot test */
    {
      if (list->alloc_values)
	{
	  free ((void *) list->value);
	}
      free ((void *) list);
    }

  return 0;
}

