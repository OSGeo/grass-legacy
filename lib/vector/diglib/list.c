/*
* $Id$
*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes.
*               Update to GRASS 5.1 Radim Blazek.
*
* PURPOSE:      Lower level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"

/* Init int_list */
int 
dig_init_list ( struct ilist *list ) 
{
    list->value = NULL;
    list->n_values = 0;
    list->alloc_values = 0;
}

/* Init add item to list */
int 
dig_list_add ( struct ilist *list, int val ) 
{
    void *p;
    int n;
    
    if ( list->n_values == list->alloc_values ) {
	n = list->n_values + 1000;
	p = realloc ( (void *) list->value, n * sizeof(int) ); 
        if ( p == NULL ) return 0;
        list->value = (int *) p;
    }
   
    list->value[list->n_values] = val;
    list->n_values++;

    return 1;
}
