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
#include <math.h>
#include "gis.h"
#include "Vect.h"

struct line_pnts *Vect__new_line_struct (void);

/*!
 \fn struct line_pnts *Vect_new_line_struct (void)
 \brief creates and initializes a struct line_pnts. This structure is
    used for reading and writing vector lines and polygons.  The library
   routines handle all memory allocation.  If 3 lines in memory are needed
   at the same time, then  simply 3 line_pnts structures have to be used
 \return  struct line_pnts *  or NULL on error
 \param void
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

/*!
 \fn int Vect_destroy_line_struct (struct line_pnts *p)
 \brief frees all memory associated with a struct line_pnts, including the struct itself
 \return no return value
 \param  line_pnts * structure
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

/*!
 \fn int Vect_copy_xyz_to_pnts (
	struct line_pnts *Points, double *x, double *y, double *z, int n)
 \brief ADD
 \return 0 on success, -1 on out of memory
 \param  line_pnts * structure, x, y, z, number of points
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


/*!
 \fn int Vect_reset_line (struct line_pnts *Points)
 \brief ADD. Make sure line structure is clean to be re-used, i.e. it
   has no points associated with it Points must have previously been
   created with Vect_new_line_struct ()
 \return no return value
 \param  line_pnts * structure
*/
/* NEW after 4.0 alpha */
int 
Vect_reset_line (struct line_pnts *Points)
{
  Points->n_points = 0;

  return 0;
}

/*!
 \fn int Vect_append_point (struct line_pnts *Points, double x, double y, double z)
 \brief appends one point to the end of a Points structure returns new
   number of points or -1 on out of memory Note, this will append to
   whatever is in line struct.  If you are re-using a line struct, be
   sure to clear out old data first by calling Vect_reset_line ()
 \return number of points
 \param line_pnts * structure, x, y, z
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

/*!
 \fn int Vect_line_insert_point (struct line_pnts *Points, int index, double x, double y, double z)
 \brief insert new point at index position and move all old points at that position and above up
 \return number of points or -1 on error (alocation)
 \param Points line structure
 \param index (from 0 to Points->n_points-1)
 \param x 
 \param y
 \param z
*/
int 
Vect_line_insert_point (struct line_pnts *Points, int index, double x, double y, double z)
{
  register int n;

  if ( index < 0 || index > Points->n_points-1 )
      G_fatal_error ("Index out of range in Vect_line_insert_point()");
  
  if (0 > dig_alloc_points (Points, Points->n_points + 1)) 
      return (-1);

  /* move up */
  for ( n = Points->n_points; n > index; n-- ) {
      Points->x[n] = Points->x[n-1];
      Points->y[n] = Points->y[n-1];
      Points->z[n] = Points->z[n-1];
  }

  Points->x[index] = x;
  Points->y[index] = y;
  Points->z[index] = z;
  return ++(Points->n_points);
}

/*!
 \fn int Vect_line_delete_point (struct line_pnts *Points, int index)
 \brief delete point at given index and move all points above down
 \return number of points
 \param Points line structure
 \param index (from 0 to Points->n_points-1)
*/
int 
Vect_line_delete_point (struct line_pnts *Points, int index)
{
  register int n;

  if ( index < 0 || index > Points->n_points-1 )
      G_fatal_error ("Index out of range in Vect_line_delete_point()");
  
  if ( Points->n_points == 0 )
      return 0;

  /* move down */
  for ( n = index; n < Points->n_points - 1; n++ ) {
      Points->x[n] = Points->x[n+1];
      Points->y[n] = Points->y[n+1];
      Points->z[n] = Points->z[n+1];
  }

  return --(Points->n_points);
}

/*!
 \fn int Vect_line_prune (struct line_pnts *Points)
 \brief remove duplicate points, i.e. zero length segments
 \return number of points
 \param Points line structure
*/
int 
Vect_line_prune (struct line_pnts *Points)
{
    int i, j;

    if ( Points->n_points > 0 ) {
	j = 1;
	for (i = 1; i < Points->n_points; i++) {
	    if ( Points->x[i] != Points->x[j-1] || Points->y[i] != Points->y[j-1] 
		 || Points->z[i] != Points->z[j-1])
	    {
		Points->x[j] = Points->x[i];
		Points->y[j] = Points->y[i];	
		Points->z[j] = Points->z[i];	
		j++;
	    }
	}
	Points->n_points = j;
    }
  
    return (Points->n_points);
}

/*!
 \fn int Vect_append_points (struct line_pnts *Points, struct line_pnts *APoints,
	           int direction)
 \brief appends points to the end of a Points structure. Note, this
   will append to whatever is in line struct.  If you are re-using a
   line struct, be sure to clear out old data first by calling
   Vect_reset_line ()
 \return new number of points, -1 on out of memory
 \param line_pnts * structure, line_pntsA * structure, direction (GV_FORWARD, GV_BACKWARD)
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


/*!
 \fn int Vect_copy_pnts_to_xyz (
	  struct line_pnts *Points, double *x, double *y, double *z, int *n)
 \brief ADD. NOTE!  x/y arrays MUST be at least as large as Points->n_points
 \return number of points copied. Also note that  n  is a pointer to int.
 \param line_pnts * structure, x, y, z
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

/*!
 \fn int Vect_point_on_line ( struct line_pnts *Points, double distance, 
	  double *x, double *y, double *z, double *angle, double *slope )
 \brief  Find point on line in the specified distance from the begining,
  measured along line.

  If the distance is greater than line length or negative, error is returned.

  ( G_begin_distance_calculations() must be called before. - Is not true
    because G_distance is not used (no 3D support) )
 
  *x, *y, *z - pointers to point coordinates or NULL
  *angle     - pointer to angle of line in that point (radians, 
               counter clockwise from x axis) or NULL
  *slope     - pointer to slope angle in radians (positive up)
 \return number of segment the point is on (first is 1),
           0 error when point is outside the line 
 \param line_pnts * structure, distance, x, y, z, angle, slope
*/
int 
Vect_point_on_line ( struct line_pnts *Points, double distance, 
	  double *x, double *y, double *z, double *angle, double *slope )
{
    int j, np, seg;
    double dist = 0, length;
    double xp, yp, zp, dx, dy, dz, dxy, dxyz, k, rest;

    G_debug ( 3, "Vect_point_on_line(): distance = %f", distance);
    if ( (distance < 0) || (Points->n_points < 2) ) return 0;

    /* Check if first or last */
    length = Vect_line_length ( Points );
    G_debug ( 3, "  length = %f", length);
    if ( distance < 0 || distance > length ) { 
        G_debug ( 3, "  -> outside line");
	return 0; 
    }
    
    np = Points->n_points;
    if ( distance == 0 ) {
        G_debug ( 3, "  -> first point");
	xp = Points->x[0];
	yp = Points->y[0];
	zp = Points->z[0];
        dx = Points->x[1] - Points->x[0];
	dy = Points->y[1] - Points->y[0];
        dz = Points->z[1] - Points->z[0];
        dxy = hypot (dx, dy); 
	seg = 1;
    } else if ( distance == length ) {
        G_debug ( 3, "  -> last point");
	xp = Points->x[np-1];
	yp = Points->y[np-1];
	zp = Points->z[np-1];
        dx = Points->x[np-1] - Points->x[np-2];
	dy = Points->y[np-1] - Points->y[np-2];
        dz = Points->z[np-1] - Points->z[np-2];
        dxy = hypot (dx, dy); 
	seg = np - 1;
    } else {
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
		seg = j + 1;
		break;
	    }
	}
    }

    if ( x != NULL ) *x = xp;
    if ( y != NULL ) *y = yp;
    if ( z != NULL ) *z = zp;

    /* calculate angle */
    if ( angle != NULL ) *angle = atan2(dy, dx);

    /* calculate slope */
    if ( slope != NULL )  *slope = atan2(dz, dxy);

    return seg;
}

/*!
 \fn int Vect_line_segment ( struct line_pnts *InPoints, 
                double start, double end,
	        struct line_pnts *OutPoints )
 \brief  Create segment of InPoints from start to end measured along the line
         and write it to OutPoints.

  If the distance is greater than line length or negative, error is returned.

 \return  1 success
          0 error when start > length or end < 0 or start < 0 or end > length
 \param line_pnts * structure, start, end, line_pnts * structure 
*/
int 
Vect_line_segment ( struct line_pnts *InPoints, double start, double end, 
	            struct line_pnts *OutPoints )
{
    int i, seg1, seg2;
    double length, tmp;
    double x1, y1, z1, x2, y2, z2;

    G_debug ( 3, "Vect_line_segment(): start = %f, end = %f, n_points = %d", start, end, InPoints->n_points);

    Vect_reset_line (OutPoints);
    
    if ( start > end ) {
	tmp = start;
	start = end;
	end = tmp;
    }
    
    /* Check start/end */
    if ( end < 0 ) return 0; 
    length = Vect_line_length ( InPoints );
    if ( start > length ) return 0;
    
    /* Find coordinates and segments of start/end */
    seg1 = Vect_point_on_line ( InPoints, start, &x1, &y1, &z1, NULL, NULL);
    seg2 = Vect_point_on_line ( InPoints, end, &x2, &y2, &z2, NULL, NULL);

    G_debug ( 3, "  -> seg1 = %d seg2 = %d", seg1, seg2);

    if ( seg1 == 0 || seg2 == 0 ) { 
	G_warning ("Segment outside line, no segment created"); 
	return 0;
    }
    
    Vect_append_point ( OutPoints, x1, y1, z1 );

    for ( i = seg1; i < seg2; i++ ) {
	Vect_append_point ( OutPoints, InPoints->x[i], InPoints->y[i], InPoints->z[i] );
    };

    Vect_append_point ( OutPoints, x2, y2, z2 );

    return 1;
}

/*!
 \fn double Vect_line_length ( struct line_pnts *Points )
 \brief calculate line length
 \return line length
 \param line_pnts * structure
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


/*!
 \fn double Vect_line_geodesic_length ( struct line_pnts *Points )
 \brief calculate line length. If projection is LL, the length is measured along the geodesic.
 \return line length
 \param line_pnts * structure
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

/*!
\fn int Vect_line_distance (	  struct line_pnts *points,
		  double ux, double uy, double uz,
		  int    with_z,
		  double *px, double *py, double *pz,
		  double *dist,
		  double *spdist,
		  double *lpdist)
 \brief calculate line distance.  Sets (if not null):
                      px, py - point on line,
                      dist   - distance to line,
                      spdist - distance to point on line from segment beginning,
                      sldist - distance to point on line form line beginning along line
 \return nearest segment (first is 1)
 \param line_pnts * structure, x, y, z of point, flag if to use z
   coordinate, (3D calculation), point on line, distance to line,
   distance of point from segment beginning, distance of point from line
   beginning
*/
/* original dig__check_dist () in grass50 */
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


/*!
 \fn double Vect_points_distance ( double x1, double y1, double z1,
		  double x2, double y2, double z2,
		  int with_z)
 \brief distance of 2 points. with_z - use z coordinate
 \return distance
 \param x1, y1, z1, x2, y2, z2, with_z
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

/*!
 \fn int Vect_line_box ( struct line_pnts *Points, BOUND_BOX *Box )
 \brief get bounding box of line
 \return 0 on success, ADD on error
 \param line_pnts * structure, BOUND_BOX
*/
int
Vect_line_box ( struct line_pnts *Points, BOUND_BOX *Box )
{
    dig_line_box ( Points, Box );
    return 0;
}

/*!
 \fn void Vect_line_reverse ( struct line_pnts *Points )
 \brief reverse the order of vertices
 \param Points line to be changed
*/
/* reverse_line - reverse order of Points ( direction of line ) */
void Vect_line_reverse ( struct line_pnts *Points )
{
    int    i, j, np;
    double x, y, z;

    np = (int) Points->n_points/2 ;

    for ( i=0; i<np; i++) {
	j = Points->n_points - i - 1;
	x = Points->x[i];  
	y = Points->y[i];
	z = Points->z[i];
	Points->x[i] = Points->x[j];  
	Points->y[i] = Points->y[j];
	Points->z[i] = Points->z[j];
	Points->x[j] = x;
	Points->y[j] = y;
	Points->z[j] = z;
    }
}


/*!
\brief fetches category number for given vector line and field
\param vmap: Map input
\param varea: line number
\param vfield: field number
\return 0: no category, >0: category number
*/
int
Vect_get_line_cat ( struct Map_info *Map, int line, int field ) {

    static struct line_cats *cats = NULL;
    int cat, ltype;

    if ( cats == NULL ) 
	cats = Vect_new_cats_struct ();

    ltype=Vect_read_line (Map, NULL, cats, line );
    Vect_cat_get(cats, field, &cat);
    G_debug (3, "Vect_get_line_cat: display line %d, ltype %d, cat %d", line, ltype, cat);

    return cat;
}
