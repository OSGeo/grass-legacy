#include <stdlib.h>
#include "line_coords.h"

#define kSize 50

struct line_coords *
init_line_coords (void)
{
  struct line_coords *coords;
  
  coords = (struct line_coords*) malloc (sizeof (struct line_coords));
  if (coords == NULL)
    return NULL;
  
  coords->x = (double*) malloc ((kSize + 1) * sizeof (double));
  if (coords->x == NULL)
    return NULL;
  coords->y = (double*) malloc ((kSize + 1) * sizeof (double));
  if (coords->y == NULL)
    return NULL;

  coords->n_coords = 0;
  coords->alloc_coords = kSize;
  return coords;
}

int 
reset_line_coords (struct line_coords *coords)
{
  if ((coords->x == NULL) || (coords->y == NULL))
    return 0;

  coords->n_coords = 0;
  return 1;
}


int 
resize_line_coords (struct line_coords *coords, int min)
{
  int no_blocks = 0;
  double *newx, *newy;

  if ((coords->x == NULL) || (coords->y == NULL))
    return 0;

  if (min <= coords->alloc_coords)
    return 1;

  do
    {
      coords->alloc_coords += kSize;
    } while (min > coords->alloc_coords);
  no_blocks = coords->alloc_coords / kSize;
  newx = (double*) realloc (coords->x,
				 ((kSize * no_blocks) + 1) * sizeof (double));
  if (newx == NULL)
    return 0;
  newy = (double*) realloc (coords->y,
				 ((kSize * no_blocks) + 1) * sizeof (double));
  if (newy == NULL)
    return 0;

  coords->x = newx;
  coords->y = newy;
  return 1;
}


int 
free_line_coords (struct line_coords *coords)
{
  if (coords)         /* probably a moot test */
    {
      if (coords->alloc_coords)
	{
	  free (coords->x);
	  free (coords->y);
	}
      free (coords);
      return 1;
    }
  return 0;
}
