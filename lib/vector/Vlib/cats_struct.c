#include "Vect.h"
#include <stdlib.h>
#include "gis.h"

/*
   **
   **  Creates and initializes a  struct line_cats  
   **  This structure is used for reading and writing vector cats
   **
   **  The library routines handle all memory allocation.
   **
   **  returns  struct line_catss *  or NULL on error
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

/*
   **  Frees all memory associated with a struct line_cats, including
   **  the struct itself!!!
   ** 
   **  no return value;
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

/* 
   **  Set category to value; if field already exist
   **  old value is overwritten; if field does not exist 
   **  new category is append to the end 
   **
   **  field  - field
   **  cat - category 
   **
   **  returns:  new number of categories
   **            0 if no space for new category in structure, n_cats would be > GRASS_V_NCATS_MAX
   **           -1 on out of memory
   **           -2 if field out of range: 1 - GRASS_V_FIELD_MAX or cat out of range:  1 - GRASS_V_CAT_MAX
 */
int 
Vect_cat_set (struct line_cats *Cats, GRASS_V_FIELD field, GRASS_V_CAT cat)
{
  register int n;

  /* check input values */
  /* compiler may warn: 
   * comparison is always 0 due to limited range of data type
   * but remember that limit is set to portable data type length
   * and machine native size may be longer */
  /*
  if (field < 1 || field > GRASS_V_FIELD_MAX || cat < 1 || cat > GRASS_V_CAT_MAX)
    return (-2);
   */
    
  /* go through old cats and find if category number exist */
  for (n = 0; n < Cats->n_cats; n++)
    {
      if (Cats->field[n] == field)
	{
	  Cats->cat[n] = cat;
	  return (1);
	}
    }

  /* field was not found so we shall append new cat */
  /* test if space exist */
  if (n >= GRASS_V_NCATS_MAX)
    return (0);

  if (0 > dig_alloc_cats (Cats, Cats->n_cats + 1))
    return (-1);

  n = Cats->n_cats;
  Cats->field[n] = field;
  Cats->cat[n] = cat;
  Cats->n_cats++;
  return (1);
}

/* 
   **  Get category 
   **
   **  field  - input field
   **  cat - output category 
   **
   **  returns:  1 found
   **            0 field does not exist
 */
int 
Vect_cat_get (struct line_cats *Cats, GRASS_V_FIELD field, GRASS_V_CAT * cat)
{
  register int n;

  /* check input value */
  /*
  if (field < 1 || field > GRASS_V_FIELD_MAX)
    return (0);
  */
    
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

/* 
   **  Delete category 
   **
   **  field  - input field
   **
   **  returns:  1 deleted
   **            0 category number does not exist
 */
int 
Vect_cat_del (struct line_cats *Cats, GRASS_V_FIELD field)
{
  register int n;

  /* check input value */
  /*
  if (field < 1 || field > GRASS_V_FIELD_MAX)
    return (0);
   */
    
  /* go through cats and find if field exist */
  for (n = 0; n < Cats->n_cats; n++)
    {
      if (Cats->field[n] == field)
	{
	  for (n; n < Cats->n_cats - 1; n++)
	    {
	      Cats->field[n] = Cats->field[n + 1];
	      Cats->cat[n] = Cats->cat[n + 1];
	    }
	  Cats->n_cats--;
	  return (1);
	}
    }

  /* field was not found */
  return (0);
}

/*
   **  Make sure cats structure is clean to be re-used.
   **  I.e. it has no cats associated with it
   **
   **  Cats must have previously been created with Vect_new_cats_struct ()
   **
   **  no return value
 */
int 
Vect_reset_cats (struct line_cats *Cats)
{
  Cats->n_cats = 0;

  return 0;
}

struct cat_list *
Vect_new_cat_list ()
{
  struct cat_list *p;

  p = (struct cat_list *) malloc (sizeof (struct cat_list));

  /* n_ranges MUST be initialized to zero */
  if (p)
    p->n_ranges = 0;

  if (p)
    p->alloc_ranges = 0;
  
  return p;
}

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


/* 
   **  Convert string of categories and cat ranges
   **  separated by commas to cat_list
   **
   **  Examples of string: 2,3,5-9,20 
   **
   **  str  - input string
   **
   **  returns:  number of errors in ranges 
   **            
 */
int 
Vect_str_to_cat_list (char *str, struct cat_list *list)
{
  int i, nr, l, err = 0;
  char *s, *e, buf[100];
  int min, max;
  
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
      list->min = (GRASS_V_CAT *) G_malloc (nr * sizeof(GRASS_V_CAT));
      list->max = (GRASS_V_CAT *) G_malloc (nr * sizeof(GRASS_V_CAT));
    }
  else if (nr > list->alloc_ranges)
    {
      list->min = (GRASS_V_CAT *) G_realloc ((void *)list->min, 
	                                nr * sizeof(GRASS_V_CAT));
      list->max = (GRASS_V_CAT *) G_realloc ((void *)list->max, 
	                                nr * sizeof(GRASS_V_CAT));
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
	  s = e + 1;
	}
      else
        {
          strcpy (buf, s);
	  s = NULL;
	}
      if ( sscanf (buf, "%d-%d", &min, &max) == 2 ) {}
      else if ( sscanf (buf, "%d", &min) == 1 )
          max = min;
      else  /* error */ 
        {
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

/* 
   **  Find if category is in list
   **
   **
   **  returns:  TRUE  if cat is in list 
   **            FALSE if it is not
 */
int 
Vect_cat_in_cat_list (GRASS_V_CAT cat, struct cat_list *list)
{
  int i;
  
  for ( i=0; i < list->n_ranges; i++)  
      if ( cat >= list->min[i] && cat <= list->max[i] )
	   return (TRUE);
	  
  return (FALSE);
}
