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
#include <string.h>
#include "gis.h"
#include "Vect.h"

static int cmp();
struct line_cats *Vect__new_cats_struct (void);

/*!
 \fn struct line_cats *Vect_new_cats_struct ()
 \brief creates and initializes a struct line_cats. This structure is
   used for reading and writing vector cats. The library routines handle
   all memory allocation.
 \return struct line_cats *  or NULL on error
 \param -
*/
struct line_cats *
Vect_new_cats_struct ()
{
  struct line_cats *p;

  if (NULL == (p = Vect__new_cats_struct ()))
    G_fatal_error ("New_line: Out of memory");

  return p;
}

struct line_cats *
Vect__new_cats_struct ()
{
  struct line_cats *p;

  p = (struct line_cats *) malloc (sizeof (struct line_cats));

  /* n_cats MUST be initialized to zero */
  if (p)
    p->n_cats = 0;

  if (p)
    p->alloc_cats = 0;
  
  return p;
}

/*!
 \fn int Vect_destroy_cats_struct (struct line_cats *p)
 \brief frees all memory associated with a struct line_cats, including the struct itself
 \return 0
 \param line_cats structure
*/
int 
Vect_destroy_cats_struct (struct line_cats *p)
{
  if (p)			/* probably a moot test */
    {
      if (p->n_cats)
	{
	  free ((char *) p->field);
	  free ((char *) p->cat);
	}
      free ((char *) p);
    }

  return 0;
}

/*!
 \fn int Vect_cat_set (struct line_cats *Cats, int field, int cat)
 \brief add new field/cat to category structure if doesn't exist yet.
 \return new number of categories
               0 if no space for new category in structure, n_cats would be > GV_NCATS_MAX
              -1 on out of memory
              -2 if field out of range: 1 - GV_FIELD_MAX or cat out of range:  1 - GV_CAT_MAX
 \param Cats
 \param field
 \param cat
*/
int 
Vect_cat_set (struct line_cats *Cats, int field, int cat)
{
  register int n;

  /* check input values */
  /* compiler may warn: 
   * comparison is always 0 due to limited range of data type
   * but remember that limit is set to portable data type length
   * and machine native size may be longer */
  /*
  if (field < 1 || field > GV_FIELD_MAX || cat < 1 || cat > GV_CAT_MAX)
    return (-2);
   */
    
  /* go through old cats and find if field/category exists */
  for (n = 0; n < Cats->n_cats; n++) {
      if (Cats->field[n] == field && Cats->cat[n] == cat )
	  return (1);
  }

  /* field was not found so we shall append new cat */
  /* test if space exist */
  if (n >= GV_NCATS_MAX)
    return (0);

  if (0 > dig_alloc_cats (Cats, Cats->n_cats + 1))
    return (-1);

  n = Cats->n_cats;
  Cats->field[n] = field;
  Cats->cat[n] = cat;
  Cats->n_cats++;
  return (1);
}

/*!
 \fn int Vect_cat_get (struct line_cats *Cats, int field, int *cat)
 \brief get first found category of given field
 \return 1 found, 0 field does not exist
 \param Cats
 \param field
 \param cat pointer to variable where cat will be written
*/
int 
Vect_cat_get (struct line_cats *Cats, int field, int *cat)
{
  register int n;

  /* check input value */
  /*
  if (field < 1 || field > GV_FIELD_MAX)
    return (0);
  */

  *cat = 0;
    
  /* go through cats and find if field exist */
  for (n = 0; n < Cats->n_cats; n++)
    {
      if (Cats->field[n] == field)
	{
	  *cat = Cats->cat[n];
	  return (1);
	}
    }

  /* field was not found */
  return (0);
}

/*!
 \fn int Vect_cat_del (struct line_cats *Cats, int field)
 \brief delete all categories of given field
 \return 1 deleted, 0 category number does not exist
 \param Cats
 \param field
*/
int 
Vect_cat_del (struct line_cats *Cats, int field)
{
  register int n, found = 0;

  /* check input value */
  /*
  if (field < 1 || field > GV_FIELD_MAX)
    return (0);
   */
    
  /* go through cats and find if field exist */
  for (n = 0; n < Cats->n_cats; n++) {
      if (Cats->field[n] == field) {
	  for (n = n; n < Cats->n_cats - 1; n++) {
	      Cats->field[n] = Cats->field[n + 1];
	      Cats->cat[n] = Cats->cat[n + 1];
	  }
	  Cats->n_cats--;
	  found = 1;
      }
  }

  return (found);
}

/*!
 \fn int Vect_field_cat_del (struct line_cats *Cats, int field, int cat)
 \brief delete field/cat from line_cats structure
 \return 1 deleted, 0 field/category number does not exist
 \param Cats
 \param field
 \param cat category to be deleted or -1 to delete all cats of given field
*/
int 
Vect_field_cat_del (struct line_cats *Cats, int field, int cat)
{
  register int n, m, found = 0;

  /* check input value */
  /*
  if (field < 1 || field > GV_FIELD_MAX)
    return (0);
   */
    
  /* go through cats and find if field exist */
  for (n = 0; n < Cats->n_cats; n++) {
      if (Cats->field[n] == field && ( Cats->cat[n] == cat || cat == -1) ) {
	  for (m = n; m < Cats->n_cats - 1; m++) {
	      Cats->field[m] = Cats->field[m + 1];
	      Cats->cat[m] = Cats->cat[m + 1];
	  }
	  Cats->n_cats--;
	  found = 1;
      }
  }

  return (found);
}

/*!
 \fn int Vect_reset_cats (struct line_cats *Cats)
 \brief reset category structure to make sure cats structure is clean
   to be re-used. I.e. it has no cats associated with it. Cats must have
   previously been created with Vect_new_cats_struct()
 \return 0
 \param line_cats structure
*/
int 
Vect_reset_cats (struct line_cats *Cats)
{
  Cats->n_cats = 0;

  return 0;
}

/*!
 \fn struct cat_list *Vect_new_cat_list ()
 \brief ADD TEXT
 \return ADD
 \param -
*/
struct cat_list *
Vect_new_cat_list ()
{
  struct cat_list *p;

  p = (struct cat_list *) malloc (sizeof (struct cat_list));

  /* n_ranges MUST be initialized to zero */
  if (p) {
    p->n_ranges = 0;
    p->alloc_ranges = 0;
    p->field = 0;
    p->min = NULL;
    p->max = NULL;
  }
  
  return p;
}


/*!
 \fn int Vect_destroy_cat_list (struct cat_list *p)
 \brief ADD TEXT
 \return ADD
 \param line_cats structure
*/
int 
Vect_destroy_cat_list (struct cat_list *p)
{
  if (p)			/* probably a moot test */
    {
      if (p->n_ranges)
	{
	  free ((char *) p->min);
	  free ((char *) p->max);
	}
      free ((char *) p);
    }

  return 0;
}


/*!
 \fn int Vect_str_to_cat_list (char *str, struct cat_list *list)
 \brief Convert string of categories and cat ranges separated by
   commas to cat_list. Examples of string: 2,3,5-9,20. str - input
   string
 \return number of errors in ranges
 \param string, line_cats structure
*/
int 
Vect_str_to_cat_list (char *str, struct cat_list *list)
{
  int i, nr, l, err = 0;
  char *s, *e, buf[100];
  int min, max;
  
  G_debug (3, "Vect_str_to_cat_list(): str = %s", str);
  
  list->n_ranges = 0;
  l = strlen (str); 
  
  /* find number of ranges */
  nr = 1; /* one range */
  for ( i=0; i < l; i++)  
      if (str[i] == ',')
	   nr++;
	  
  /* allocate space */
  if ( list->alloc_ranges == 0 )
    {	    
      list->min = (int *) G_malloc (nr * sizeof(int));
      list->max = (int *) G_malloc (nr * sizeof(int));
    }
  else if (nr > list->alloc_ranges)
    {
      list->min = (int *) G_realloc ((void *)list->min, 
	                                nr * sizeof(int));
      list->max = (int *) G_realloc ((void *)list->max, 
	                                nr * sizeof(int));
    }
    
  /* go through string and read ranges */
  i = 0;  
  s = str;  
  
  while (s)
    {
      e = (char *) strchr (s, ','); /* first comma */
      if( e )
        {
          l = e - s;
          strncpy (buf, s, l);
	  buf[l] = '\0';
	  s = e + 1;
	}
      else
        {
          strcpy (buf, s);
	  s = NULL;
	}
      
      G_debug (3, "  buf = %s", buf);
      if ( sscanf (buf, "%d-%d", &min, &max) == 2 ) {}
      else if ( sscanf (buf, "%d", &min) == 1 )
          max = min;
      else  /* error */ 
        {
	  G_warning ("Cannot convert category string '%s' (from '%s') to category range", buf, str);
	  err++;	
	  continue;
        }
  
      list->min[i] = min;  
      list->max[i] = max;
      i++;
    }  
      
  list->n_ranges = i;
  
  return (err);
}

/*!
 \fn int Vect_array_to_cat_list (int *vals, int nvals, struct cat_list *list)
 \brief Convert ordered array of integers to cat_list
 \return number of ranges
 \param array of integers ?, number of values ?, line_cats structure
*/
int
Vect_array_to_cat_list (int *vals, int nvals, struct cat_list *list)
{
    int i, range;

    G_debug (1, "Vect_array_to_cat_list()");
    range = -1;
    for (i = 0; i < nvals; i++)
      {
	if ( i == 0 || (vals[i] - list->max[range]) > 1 )
	  {
            range++;
            if ( range == list->alloc_ranges)
              {
	        list->alloc_ranges += 1000;	  
                list->min = (int *) G_realloc ((void *)list->min, 
	                                list->alloc_ranges * sizeof(int));
                list->max = (int *) G_realloc ((void *)list->max, 
	                                list->alloc_ranges * sizeof(int));
              }
	    list->min[range] = vals[i];
	    list->max[range] = vals[i];
	  }
	else
	  {
	    list->max[range] = vals[i];
	  }
      }
    
    list->n_ranges = range+1;

    return (list->n_ranges);
}

/*!
 \fn int Vect_cat_in_cat_list (int cat, struct cat_list *list)
 \brief check if category is in list
 \return TRUE  if cat is in list, FALSE if it is not
 \param category number, line_cats structure
*/
int 
Vect_cat_in_cat_list (int cat, struct cat_list *list)
{
  int i;
  
  for ( i=0; i < list->n_ranges; i++)  
      if ( cat >= list->min[i] && cat <= list->max[i] )
	   return (TRUE);
	  
  return (FALSE);
}

/*!
 \fn int Vect_cat_in_array (int cat, int *array, int ncats)
 \brief check if category is in ordered array of integers
 \return TRUE  if cat is in list, FALSE if it is not
 \param category number, ordered array of integers, line_cats structure
*/
int 
Vect_cat_in_array (int cat, int *array, int ncats)
{
  int *i;
  
  i = bsearch ( (void *) &cat, (void *) array, ncats,
		sizeof (int), cmp);

  if ( i != NULL ) return (TRUE);
  
  return (FALSE);
}


int cmp ( const void *pa, const void *pb)
{
    int *p1 = (int *) pa;
    int *p2 = (int *) pb;
	 
    if( *p1 < *p2 )
       return -1;
    if( *p1 > *p2 )
       return 1;
    return 0;
}
