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
#include "Vect.h"
#include "gis.h"
/*
   **  These routines can affect the read_next_line funtions
   **    by restricting what they return.
   **  They are applied on a per map basis.
   **
   **  These do not affect the lower level direct read functions.
 */


/*!
 \fn int Vect_set_constraint_region ( struct Map_info *Map,
			     double n, double s, double e, double w, double t, double b )
 \brief ADD
 \return 0 on success, -1 on error
 \param Map_info structure, north, south, east, west, top, bottom coordinates
*/
int 
Vect_set_constraint_region ( struct Map_info *Map,
			     double n, double s, double e, double w, double t, double b )
{
  if (n <= s)
    return (-1);
  if (e <= w)
    return (-1);

  Map->Constraint_region_flag = 1;
  Map->Constraint_N = n;
  Map->Constraint_S = s;
  Map->Constraint_E = e;
  Map->Constraint_W = w;
  Map->Constraint_T = t;
  Map->Constraint_B = b;
  Map->proj = G_projection ();

  return (0);
}				/*  dig_init_box()  */

/*!
 \fn int Vect_get_constraint_box ( struct Map_info *Map,
			      BOUND_BOX *Box )
 \brief ADD
 \return 0 on success
 \param Map_info structure, BOUND_BOX
*/
int 
Vect_get_constraint_box ( struct Map_info *Map,  BOUND_BOX *Box )
{
    Box->N = Map->Constraint_N;
    Box->S = Map->Constraint_S;
    Box->E = Map->Constraint_E;
    Box->W = Map->Constraint_W;
    Box->T = Map->Constraint_T;
    Box->B = Map->Constraint_B;

    return 0;
}
/*
   **  Normally, All 'Alive' lines will be returned unless overridden
   **  by this function.   You can specified all the types you are interested
   **   in  (by oring their types together).  You can use this to say 
   **  exclude Area type lines.
   **
   **   By default all DEAD lines are ignored by the read_next_line () functions
   **  This too can be overridden by including their types.
   **
   **  Refer to  dig_defines for the line type Defines
   **
   **   All lines can be forced to be read by setting type = -1
   **
 */

/*!
 \fn int Vect_set_constraint_type (
			   struct Map_info *Map,
			   int type)
 \brief ADD
 \return 0 on success
 \param Map_info structure, constraint type
*/
int 
Vect_set_constraint_type (
			   struct Map_info *Map,
			   int type)
{
  Map->Constraint_type = type;
  Map->Constraint_type_flag = 1;

  return 0;
}

/*!
 \fn int Vect_remove_constraints (struct Map_info *Map)
 \brief ADD
 \return 0 on success
 \param Map_info structure
*/
int 
Vect_remove_constraints (struct Map_info *Map)
{
  Map->Constraint_region_flag = 0;
  Map->Constraint_type_flag = 0;

  return 0;
}
