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
#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"

struct line_pnts *Vect__new_line_struct (void);

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
  
  if (p)
    p->x = p->y = p->z = NULL;

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
      if ( z != NULL ) 
          Points->z[i] = z[i];
      else
          Points->z[i] = 0;
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
int 
Vect_append_point (struct line_pnts *Points, double x, double y, double z)
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
   ** Appends points to the end of a Points structure
   ** returns new number of points or -1 on out of memory
   **
   **  Note, this will append to whatever is in line struct.
   **   If you are re-using a line struct, be sure to clear out old 
   **   data first by calling Vect_reset_line ()
 */
int 
Vect_append_points (struct line_pnts *Points, struct line_pnts *APoints,
	           int direction) /* GV_FORWARD, GV_BACKWARD */
{
  int i, n, on, an;

  on = Points->n_points;
  an = APoints->n_points;
  n = on + an;
  
  /* Should be OK, dig_alloc_points calls realloc */
  if (0 > dig_alloc_points (Points, n)) 
    return (-1);
  
  if ( direction == GV_FORWARD ) {
      for (i = 0; i < an; i++) {
          Points->x[on + i] = APoints->x[i];
          Points->y[on + i] = APoints->y[i];
          Points->z[on + i] = APoints->z[i];
      }
  } else {
      for (i = 0; i < an; i++) {
          Points->x[on + i] = APoints->x[an - i - 1];
          Points->y[on + i] = APoints->y[an - i - 1];
          Points->z[on + i] = APoints->z[an - i - 1];
      }
  }
  
  Points->n_points = n;
  return n;
}

/*
   ** returns number of points copied
   **
   ** NOTE!  x/y arrays MUST be at least as large as Points->n_points
   **
   **   Also note that  n  is a pointer to int.
 */

int 
Vect_copy_pnts_to_xyz (
	  struct line_pnts *Points, double *x, double *y, double *z, int *n)
{
  register int i;

  for (i = 0; i < *n; i++)
    {
      x[i] = Points->x[i];
      y[i] = Points->y[i];
      if ( z != NULL )
          z[i] = Points->z[i];
      *n = Points->n_points;
    }

  return (Points->n_points);
}

/* Find point on line in the specified distance from the begining,
*  measured along line.
*
*  If the distance is greater than line length or negative, error is returned.
*
*  ( G_begin_distance_calculations() must be called before. - Is not true
*    because G_distance is not used (no 3D support) )
* 
*  *x, *y, *z - pointers to point coordinates or NULL
*  *angle     - pointer to angle of line in that point (radians, 
*               counter clockwise from x axis) or NULL
*  *slope     - pointer to slope angle in radians (positive up)
* 
*  Returns: number of segment the point is on (first is 1)
*           0 error - point is outside the line 
*/
int 
Vect_point_on_line ( struct line_pnts *Points, double distance, 
	  double *x, double *y, double *z, double *angle, double *slope )
{
    int j;
    double dist = 0;
    double xp, yp, zp, dx, dy, dz, dxy, dxyz, k, rest;

    if ( (distance < 0) || (Points->n_points < 2) ) return 0;
    
    for ( j = 0; j < Points->n_points - 1; j++) {
        /* dxyz = G_distance(Points->x[j], Points->y[j], 
	                     Points->x[j+1], Points->y[j+1]); */
	dx = Points->x[j+1] - Points->x[j];
        dy = Points->y[j+1] - Points->y[j];
        dz = Points->z[j+1] - Points->z[j];
        dxy = hypot (dx, dy); 
        dxyz = hypot (dxy, dz); 
	
	dist += dxyz; 
        if ( dist >= distance ){ /* point is on the current line part */
            rest = distance - dist + dxyz; /* from first point of segment to point */
            k = rest / dxyz;

	    xp = Points->x[j] + k * dx;
            yp = Points->y[j] + k * dy;
            zp = Points->z[j] + k * dz;
	
            if ( x != NULL ) *x = xp;
            if ( y != NULL ) *y = yp;
            if ( z != NULL ) *z = zp;
	    
	    /* calculate angle */
	    if ( angle != NULL )
	       *angle = atan2(dy, dx);

	    /* calculate slope */
	    if ( slope != NULL )
	       *slope = atan2(dz, dxy);
	    
	    return (j + 1);
	}
    }

    return 0;
}

/* Calculate line length
* 
*  Returns: line length
*/
double 
Vect_line_length ( struct line_pnts *Points )
{
    int j;
    double dx, dy, dz, len = 0;

    if ( Points->n_points < 2 ) return 0;
    
    for ( j = 0; j < Points->n_points - 1; j++) {
	dx = Points->x[j+1] - Points->x[j];
        dy = Points->y[j+1] - Points->y[j];
        dz = Points->z[j+1] - Points->z[j];
        len += hypot ( hypot (dx, dy), dz ); 
    }

    return len;
}

/* Calculate line length. If projection is LL, the length is measured along the geodesic.
* 
*  Returns: line length
*/
double 
Vect_line_geodesic_length ( struct line_pnts *Points )
{
    int j, dc;
    double dx, dy, dz, dxy, len = 0;

    dc = G_begin_distance_calculations();

    if ( Points->n_points < 2 ) return 0;
    
    for ( j = 0; j < Points->n_points - 1; j++) {
	if ( dc == 2 ) 
	    dxy = G_geodesic_distance ( Points->x[j],  Points->y[j],  Points->x[j+1],  Points->y[j+1] );
	else {
	    dx = Points->x[j+1] - Points->x[j];
	    dy = Points->y[j+1] - Points->y[j];
            dxy = hypot (dx, dy);
	} 
	    
        dz = Points->z[j+1] - Points->z[j];
        len += hypot ( dxy, dz ); 
    }

    return len;
}

/* original dig__check_dist () in grass50 
*  
*  returns: nearest segment (first is 1)
*  sets (if not null): px, py - point on line
*                      dist   - distance to line
*                      spdist - distance to point on line from segment beginning
*                      sldist - distance to point on line form line beginning along line
*/  
int 
Vect_line_distance (
		  struct line_pnts *points, /* line */
		  double ux, double uy, double uz,    /* point */
		  int    with_z,   /* use z coordinate, (3D calculation) */
		  double *px, double *py, double *pz, /* point on line */
		  double *dist,             /* distance to line */
		  double *spdist,           /* distance of point from segment beginning */
		  double *lpdist)           /* distance of point from line beginning */
{
  register int i;
  register double distance;
  register double new_dist;
  double   tpx, tpy, tpz, tdist, tspdist, tlpdist;
  double dx, dy, dz;
  register int n_points;
  int segment;

  n_points = points->n_points;

  if ( n_points == 1 ) {
    distance = dig_distance2_point_to_line (ux, uy, uz, points->x[0], points->y[0], points->z[0],
              				                points->x[0], points->y[0], points->z[0],
							with_z,
				  	                NULL, NULL, NULL, NULL, NULL);
    tpx = points->x[0];
    tpy = points->y[0];
    tpz = points->z[0];
    tdist = sqrt(distance);
    tspdist = 0;
    tlpdist = 0;
    segment = 0;
    
  } else {

      distance = dig_distance2_point_to_line (ux, uy, uz, points->x[0], points->y[0], points->z[0],
						          points->x[1], points->y[1], points->z[1],
							  with_z,
						          NULL, NULL, NULL, NULL, NULL);
      segment = 1;
	  
      for ( i = 1 ; i < n_points - 1; i++)
	{
	  new_dist = dig_distance2_point_to_line (ux, uy, uz, 
		                                  points->x[i], points->y[i], points->z[i],
					          points->x[i + 1], points->y[i + 1], points->z[i + 1],
					          with_z,
					          NULL, NULL, NULL, NULL, NULL);
	  if (new_dist < distance)
	    {
	      distance = new_dist;
	      segment = i + 1;
	    }
	}

      /* we have segment and now we can recalculate other values (speed) */
      new_dist = dig_distance2_point_to_line (ux, uy, uz, 
	                       points->x[segment - 1], points->y[segment - 1], points->z[segment - 1],
			       points->x[segment], points->y[segment], points->z[segment],
			       with_z,
			       &tpx, &tpy, &tpz, &tspdist, NULL);
      
      /* calculate distance from beginning of line */
      if ( lpdist ) { 
          tlpdist = 0;
	  for ( i = 0; i < segment - 1; i++) {
              dx = points->x[i+1] - points->x[i];
	      dy = points->y[i+1] - points->y[i];
	      if ( with_z ) 
	          dz = points->z[i+1] - points->z[i];
	      else 
		  dz = 0;

	      tlpdist += hypot ( hypot (dx, dy), dz );
	  }
	  tlpdist += tspdist;
      }
      tdist = sqrt(distance);
  }
  
  if (px) *px = tpx;
  if (py) *py = tpy;
  if (pz && with_z) *pz = tpz;
  if (dist) *dist = tdist;
  if (spdist) *spdist = tspdist;
  if (lpdist) *lpdist = tlpdist;
  
  return (segment);
}

/* Distance of 2 points. 
*  
*  with_z - use z coordinate
*  
*  returns: distance
*/  
double
Vect_points_distance (
		  double x1, double y1, double z1,    /* point 1 */
		  double x2, double y2, double z2,    /* point 2 */
		  int with_z)
{
    double dx, dy, dz;


    dx = x2 - x1;
    dy = y2 - y1;
    dz = z2 - z1;

    if (with_z)
        return hypot ( hypot (dx, dy), dz );
    else
        return hypot (dx, dy);
  
}

/* Line bounding box.  */
int
Vect_line_box ( struct line_pnts *Points, BOUND_BOX *Box )
{
    dig_line_box ( Points, Box );
    return 0;
}

