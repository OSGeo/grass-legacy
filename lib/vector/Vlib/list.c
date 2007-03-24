/**
 * \file list.c
 *
 * \brief Vector library
 *
 * Higher level functions for reading/writing/manipulating vectors.
 *
 * This program is free software under the GNU General Public
 * License (>=v2). Read the file COPYING that comes with GRASS
 * for details.
 *
 * \author Original author CERL, probably Dave Gerdes or Mike Higgins.
 * Update to GRASS 5.7 Radim Blazek and David D. Gray
 */

/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes or Mike Higgins.
*               Update to GRASS 5.7 Radim Blazek and David D. Gray.
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
#include <stdlib.h>
#include <grass/Vect.h>
#include <grass/gis.h>

/* ADD comment what and why a list */

/**
 * \fn struct ilist *Vect_new_list (void)
 *
 * \brief Creates and initializes a struct ilist.
 *
 * This structure is used as container for integer values. The
 * library routines handle all memory allocation.
 *
 * \return pointer to struct ilist
 * \return NULL on error
 */
struct ilist *
Vect_new_list (void)
{
  struct ilist *p;

  p = (struct ilist *) G_malloc (sizeof (struct ilist));

  if (p) {
    p->value = NULL;
    p->n_values = 0;
    p->alloc_values = 0;
  }
  
  return p;
}

/**
 * \fn int Vect_reset_list (struct ilist *list)
 *
 * \brief Reset ilist structure to make sure ilist structure is clean
 * to be re-used. List must have previously been created with
 * Vect_new_list()
 * 
 * \return 0
 *
 * \param[in,out] list pointer to struct ilist
 */
int 
Vect_reset_list (struct ilist *list)
{
  list->n_values = 0;

  return 0;
}

/**
 * \fn int Vect_destroy_list (struct ilist *list)
 *
 * \brief Frees all memory associated with a struct ilist, including
 * the struct itself
 *
 * \return 0
 *
 * \param[in,out] list pointer to ilist structure
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

/**
 * \fn int Vect_list_append (struct ilist *list, int val )
 *
 * \brief Append new item to the end of list if not yet present 
 *
 * \return 0 on success
 * \return 1 on error
 *
 * \param[in,out] list pointer to ilist structure
 * \param[in] val new item to append to the end of list
 */
int
Vect_list_append ( struct ilist *list, int val )
{
    int i;
    size_t size;
    
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

/**
 * \fn int Vect_list_append_list (struct ilist *alist,  struct ilist *blist )
 *
 * \brief Append new items to the end of list if not yet present 
 *
 * \return 0 on success
 * \return 1 on error
 *
 * \param[in,out] alist pointer to ilist structure where items will be appended
 * \param[in] blist pointer to ilist structure with new items
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

/**
 * \fn int Vect_list_delete (struct ilist *list, int val )
 *
 * \brief Remove a given value (item) from list
 *
 * \return 0 on success
 * \return 1 on error
 *
 * \param[in,out] list pointer to ilist structure
 * \param[in] val to remove
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

/**
 * \fn int Vect_list_delete_list ( struct ilist *alist,  struct ilist *blist )
 *
 * \brief Delete list from existing list 
 *
 * \return 0 on success
 * \return 1 on error
 *
 * \param[in,out] alist pointer to original ilist structure,
 * \param[in] blist pointer to ilist structure with items to delete
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

/**
 * \fn int Vect_val_in_list ( struct ilist *list, int val )
 *
 * \brief Find a given item in the list
 *
 * \return 1 if an item is found
 * \return 0 no found item in the list
 *
 * \param[in] list pointer to ilist structure
 * \param[in] val value of item
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
