#include <limits.h>
#include <math.h>
#include "distance.h"
int
nearest_node(x,y,map,near_node_num)
 double y,x;
 struct Map_info *map;
 int *near_node_num;
{
  double shortest_dist2;
  double distance2;
  double pt_x1,pt_y1,pt_x2,pt_y2;
  int i;
  int found;
  double x2, y2;
  extern int pt_in_WIND();

  found = 0;
  if (pt_in_WIND(y,x)!=1)
    return(found);
  shortest_dist2 = DBL_MAX;
  pt_x1 = x;
  pt_y1 = y;
  for (i=1; i <= map->n_nodes; i++)
   {
    if (map->Node[i].alive==1)
     {
      if ((map->Node[i].y >= window.south) &&
          (map->Node[i].y <= window.north) &&
          (map->Node[i].x >= window.west)  &&
          (map->Node[i].x <= window.east)    )
       {
        pt_x2 = map->Node[i].x;
        pt_y2 = map->Node[i].y;
        x2 = pow((pt_x1-pt_x2),(double)2.0);
        y2 = pow((pt_y1-pt_y2),(double)2.0);
        distance2 = x2 + y2;
        if (distance2 < shortest_dist2)
         {
          shortest_dist2 = distance2;
          *near_node_num = i;
          found = 1;
         }
       }
     }
   }
  return(found);
 }
