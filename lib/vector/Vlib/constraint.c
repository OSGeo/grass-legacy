#include "Vect.h"
#include "gis.h"

/*
   **  These routines can affect the read_next_line funtions
   **    by restricting what they return.
   **  They are applied on a per map basis.
   **
   **  These do not affect the lower level direct read functions.
 */

int 
Vect_set_constraint_region (
			     struct Map_info *Map,
			     double n, double s, double e, double w)
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
  Map->proj = G_projection ();

  return (0);
}				/*  dig_init_box()  */



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

int 
Vect_set_constraint_type (
			   struct Map_info *Map,
			   int type)
{
  Map->Constraint_type = type;
  Map->Constraint_type_flag = 1;

  return 0;
}

int 
Vect_remove_constraints (struct Map_info *Map)
{
  Map->Constraint_region_flag = 0;
  Map->Constraint_type_flag = 0;

  return 0;
}
