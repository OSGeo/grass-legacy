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
	  G_free ((void *) list->value);
	}
      G_free ((void *) list);
    }
  list = NULL;

  return 0;
}

/* Append new item to the end of list 
*
*  returns: 0 - OK
*           1 - error
*/
int
Vect_list_append ( struct ilist *list, int val )
{
    int size;
    
    if ( list == NULL ) 
        return 1;
	
    if ( list->n_values == list->alloc_values ) {
	size = list->n_values + 1000;
        list->value = (int *) G_realloc ( (void *) list->value, size );
        list->alloc_values = size;
    }
    
    list->value[list->n_values] = val;
    list->n_values++;
  
    return 0;
}

