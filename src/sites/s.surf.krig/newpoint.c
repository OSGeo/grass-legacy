#include "gis.h"
#include "krig.h"
/*
 * newpoint function was taken (without modification) from s.surf.idw
 * Parameters passed: z,east,north all doubles
 */

/*
 * What this does: This is an unmodified routine taken from s.surf.idw. It
 * dynamically allocates memory for each new point read in by the
 * read.sites program.
 */

int newpoint (double z, double east, double north)
{
  extern int npoints, npoints_alloc;
  extern Point *points;

  if (points == NULL)
    {
      points = (Point *) G_calloc (128, sizeof (Point));
      npoints_alloc = 128;      
    }
  else if (npoints_alloc <= npoints)
    {
      points = (Point *) G_realloc (points, (npoints_alloc + 128) * sizeof (Point));
      npoints_alloc += 128;
    }
  points[npoints].north = north;
  points[npoints].east = east;
  points[npoints].z = z;
  npoints++;

  return 0;
}
