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
#include "Vect.h"

/*
   **  returns the polygon array of points  in BPoints
   **   returns  number of points or -1 on error
 */

static int first_time = 1;	/* zero at startup */
static struct line_pnts Points;


int 
Vect_get_area_points (
		       struct Map_info *Map,
		       int area,
		       struct line_pnts *BPoints)
{
  register int i, line;
  int start, end, to, from, inc;
  P_AREA *Area;
  int done_yet;

  BPoints->n_points = 0;
  BPoints->alloc_points = 0;
  Area = &(Map->Area[area]);

  if (first_time == 1)
    {
      Points.alloc_points = 0;	/* executed only once */
      first_time = 0;
    }

  for (i = 0; i < Area->n_lines; i++)
    {
      line = abs (Area->lines[i]);

      if (0 > V2_read_line (Map, &Points, NULL, line))
	return (-1);

      if (0 > dig_alloc_points (BPoints, Points.n_points + BPoints->n_points))
	return (-1);

      if (Area->lines[i] < 0)
	{
	  start = Points.n_points - 1;
	  inc = -1;
	  end = 1;
	}
      else
	{
	  end = Points.n_points - 2;
	  inc = 1;
	  start = 0;
	}

      done_yet = 0;
      for (from = start, to = BPoints->n_points; !done_yet; from += inc, to++)
	{
	  if (from == end)
	    done_yet = 1;
	  BPoints->x[to] = Points.x[from];
	  BPoints->y[to] = Points.y[from];
	}
      BPoints->n_points = Points.n_points + BPoints->n_points - 1;

    }

  if (0 > dig_alloc_points (BPoints, BPoints->n_points + 2))
    return (-1);
  BPoints->x[BPoints->n_points] = BPoints->x[0];
  BPoints->y[BPoints->n_points] = BPoints->y[0];
  BPoints->n_points++;


  return (BPoints->n_points);
}

int 
Vect_get_isle_points (
		       struct Map_info *Map,
		       int isle,
		       struct line_pnts *BPoints)
{
  register int i, line;
  int start, end, to, from, inc;
  P_ISLE *Isle;
  int done_yet;



  BPoints->n_points = 0;
  BPoints->alloc_points = 0;
  Isle = &(Map->Isle[isle]);

  if (first_time == 1)
    {
      Points.alloc_points = 0;	/* executed only once */
      first_time = 0;
    }

  for (i = 0; i < Isle->n_lines; i++)
    {
      line = abs (Isle->lines[i]);

      if (0 > V2_read_line (Map, &Points, NULL, line))
	return (-1);

      if (0 > dig_alloc_points (BPoints, Points.n_points + BPoints->n_points))
	return (-1);

      if (Isle->lines[i] < 0)
	{
	  start = Points.n_points - 1;
	  inc = -1;
	  end = 1;
	}
      else
	{
	  end = Points.n_points - 2;
	  inc = 1;
	  start = 0;
	}

      done_yet = 0;
      for (from = start, to = BPoints->n_points; !done_yet; from += inc, to++)
	{
	  if (from == end)
	    done_yet = 1;
	  BPoints->x[to] = Points.x[from];
	  BPoints->y[to] = Points.y[from];
	}
      BPoints->n_points = Points.n_points + BPoints->n_points - 1;

    }

  if (0 > dig_alloc_points (BPoints, BPoints->n_points + 2))
    return (-1);
  BPoints->x[BPoints->n_points] = BPoints->x[0];
  BPoints->y[BPoints->n_points] = BPoints->y[0];
  BPoints->n_points++;


  return (BPoints->n_points);
}
