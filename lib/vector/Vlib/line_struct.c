#include "Vect.h"
#include <stdlib.h>
#include "gis.h"

/*
   **
   **  Creates and initializes a  struct line_pnts  
   **   This structure is used for reading and writing vector lines and
   **   polygons.
   **
   **  The library routines handle all memory allocation.
   **
   **  If you need 3 lines in memory at the same time, then you simply need
   **   to use 3 line_pnts structures.
   **
   **  returns  struct line_pnts *  or NULL on error
 */
struct line_pnts *
Vect_new_line_struct ()
{
  struct line_pnts *p;

  if (NULL == (p = Vect__new_line_struct ()))
    G_fatal_error ("New_line: Out of memory");

  return p;
}

struct line_pnts *
Vect__new_line_struct ()
{
  struct line_pnts *p;

  p = (struct line_pnts *) malloc (sizeof (struct line_pnts));

  /* alloc_points MUST be initialized to zero */
  if (p)
    p->alloc_points = p->n_points = 0;

  return p;
}

/*
   ** Frees all memory associated with a struct line_pnts, including
   **  the struct itself!!!
   ** 
   **  no return value;
 */
int 
Vect_destroy_line_struct (struct line_pnts *p)
{
  if (p)			/* probably a moot test */
    {
      if (p->alloc_points)
	{
	  free ((char *) p->x);
	  free ((char *) p->y);
	  free ((char *) p->z);
	}
      free ((char *) p);
    }

  return 0;
}

/*
   ** returns 0 or -1 on out of memory
 */
int 
Vect_copy_xy_to_pnts (
	struct line_pnts *Points, double *x, double *y, int n)
{
  register int i;

  if (0 > dig_alloc_points (Points, n))
    return (-1);

  for (i = 0; i < n; i++)
    {
      Points->x[i] = x[i];
      Points->y[i] = y[i];
      Points->n_points = n;
    }

  return (0);
}

/*
   ** returns 0 or -1 on out of memory
 */
int 
Vect_copy_xyz_to_pnts (
	struct line_pnts *Points, double *x, double *y, double *z, int n)
{
  register int i;

  if (0 > dig_alloc_points (Points, n))
    return (-1);

  for (i = 0; i < n; i++)
    {
      Points->x[i] = x[i];
      Points->y[i] = y[i];
      Points->z[i] = z[i];
      Points->n_points = n;
    }

  return (0);
}

/*
   **  Make sure line structure is clean to be re-used.
   **  I.e. it has no points associated with it
   **
   **  Points must have previously been created with Vect_new_line_struct ()
   **
   **  no return value
 */
/* NEW after 4.0 alpha */
int 
Vect_reset_line (struct line_pnts *Points)
{
  Points->n_points = 0;

  return 0;
}

/* 
   **  Appends one point to the end of a Points structure
   ** returns new number of points or -1 on out of memory
   **
   **  Note, this will append to whatever is in line struct.
   **   If you are re-using a line struct, be sure to clear out old 
   **   data first by calling Vect_reset_line ()
 */
/* NEW after 4.0 alpha */

int 
Vect_append_point (struct line_pnts *Points, double x, double y)
{
  register int n;

  if (0 > dig_alloc_points (Points, Points->n_points + 1))
    return (-1);

  n = Points->n_points;
  Points->x[n] = x;
  Points->y[n] = y;
  return ++(Points->n_points);
}

int 
Vect_append_3d_point (struct line_pnts *Points, double x, double y, double z)
{
  register int n;

  if (0 > dig_alloc_points (Points, Points->n_points + 1))
    return (-1);

  n = Points->n_points;
  Points->x[n] = x;
  Points->y[n] = y;
  Points->z[n] = z;
  return ++(Points->n_points);
}

/*
   ** returns number of points copied
   **
   ** NOTE!  x/y arrays MUST be at least as large as Points->n_points
   **
   **   Also note that  n  is a pointer to int.
 */

int 
Vect_copy_pnts_to_xy (
		     struct line_pnts *Points, double *x, double *y, int *n)
{
  register int i;

  for (i = 0; i < *n; i++)
    {
      x[i] = Points->x[i];
      y[i] = Points->y[i];
      *n = Points->n_points;
    }

  return (Points->n_points);
}

int 
Vect_copy_pnts_to_xyz (
	  struct line_pnts *Points, double *x, double *y, double *z, int *n)
{
  register int i;

  for (i = 0; i < *n; i++)
    {
      x[i] = Points->x[i];
      y[i] = Points->y[i];
      z[i] = Points->z[i];
      *n = Points->n_points;
    }

  return (Points->n_points);
}


