#include "distance.h"
int
arc_is_dot(map,p,arc)
  struct Map_info *map;
  struct line_pnts *p;
  int arc;
 {
  int is_dot;
  
  is_dot = 0;
  if (map->Line[abs(arc)].N1==map->Line[abs(arc)].N2)
   {
    is_dot = 1;
   }
  return(is_dot); 
 }
