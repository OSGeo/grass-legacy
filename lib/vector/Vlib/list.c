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
#include <stdlib.h>
#include "gis.h"

/* ADD comment what and why a list */

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

/*!
 \fn int Vect_reset_list (struct ilist *list)
 \brief ADD
 \return no return value
 \param ilist * structure
*/
int 
Vect_reset_list (struct ilist *list)
{
  list->n_values = 0;

  return 0;
}

/*!
 \fn int Vect_destroy_list (struct ilist *list)
 \brief ADD
 \return no return value
 \param ilist * structure
*/
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

/*!
 \fn int Vect_list_append (struct ilist *list, int val )
 \brief Append new item to the end of list if not yet present 
 \return 0 on success, 1 on error
 \param ilist * structure, ADD
*/
int
Vect_list_append ( struct ilist *list, int val )
{
    int i, size;
    
    if ( list == NULL ) 
        return 1;
	
    for ( i = 0; i < list->n_values; i++ ) {
	if ( val == list->value[i] )
	    return 0;
    }
    
    if ( list->n_values == list->alloc_values ) {
		size = (list->n_values + 1000) * sizeof(int);
        list->value = (int *) G_realloc ( (void *) list->value, size );
        list->alloc_values = list->n_values + 1000;
    }
    
    list->value[list->n_values] = val;
    list->n_values++;
  
    return 0;
}

/*!
 \fn int Vect_list_append_list (struct ilist *alist,  struct ilist *blist )
 \brief Append new item to the end of list if not yet present 
 \return 0 on success, 1 on error
 \param ailist * structure, bilist * structure
*/
int
Vect_list_append_list ( struct ilist *alist,  struct ilist *blist )
{
    int i;
    
    if ( alist == NULL || blist == NULL ) 
        return 1;
	
    for ( i = 0; i < blist->n_values; i++ ) 
        Vect_list_append ( alist, blist->value[i] );
    
    return 0;
}

/*!
 \fn int Vect_list_delete (struct ilist *list, int val )
 \brief  Remove value from list
 \return 0 on success, 1 on error
 \param ilist * structure, ADD
*/
int
Vect_list_delete ( struct ilist *list, int val )
{
    int i, j;
    
    if ( list == NULL ) 
        return 1;
	
    for ( i = 0; i < list->n_values; i++ ) {
	if ( val == list->value[i] ) {
            for ( j = i + 1; j < list->n_values; j++ ) 
                list->value[j - 1] = list->value[j];
		
            list->n_values--;
	    return 0;
	}
    }
    
    return 0;
}

/*!
 \fn int Vect_list_delete_list ( struct ilist *alist,  struct ilist *blist )
 \brief  Delete list from existing list 
 \return 0 on success, 1 on error
 \param ailist * structure, bilist * structure
*/
int
Vect_list_delete_list ( struct ilist *alist,  struct ilist *blist )
{
    int i;
    
    if ( alist == NULL || blist == NULL ) 
        return 1;
	
    for ( i = 0; i < blist->n_values; i++ ) 
        Vect_list_delete ( alist, blist->value[i] );
    
    return 0;
}

/*!
 \fn int Vect_val_in_list ( struct ilist *list, int val )
 \brief is value in list?
 \return 1 if in list, 0 if not in list
 \param line_pnts * structure, value
*/
int
Vect_val_in_list ( struct ilist *list, int val )
{
    int i;
    
    if ( list == NULL ) 
        return 0;
	
    for ( i = 0; i < list->n_values; i++ ) {
	if ( val == list->value[i] )
	    return 1;
    }
    
    return 0;
}
