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
#include "gis.h"
#include "Vect.h"

int 
V__map_overlap (
		 struct Map_info *Map,
		 double n, double s,
		 double e, double w)
{
  struct Cell_head W;

  /* updated for Lat lon support 21 Jun 91 */
  W.north = Map->Constraint_N;
  W.south = Map->Constraint_S;
  W.east = Map->Constraint_E;
  W.west = Map->Constraint_W;
  W.proj = Map->proj;

  return G_window_overlap (&W, n, s, e, w);
}

