#include "distance.h"
#include <math.h>
#ifdef __CYGWIN__
#define HUGE HUGE_VAL
#endif
int
nearest_seg(map,p,arc,x_pt,y_pt,near_seg,shortest_dist)
 struct Map_info *map;
 struct line_pnts *p;
 int arc;
 double x_pt, y_pt;
 int *near_seg;
 double *shortest_dist;
 {
  int i;
  double dist;
  double x1,y1,x2,y2;
  extern short int clip();
  short int accept;
  int seg_found;
  char seg_ind, seg_type;
  double x, y;
  double xmin,xmax,ymin,ymax;

  seg_found = 0;
  if (pt_in_WIND(y_pt,x_pt)!=1)
   {
    return(seg_found);
   }
  *shortest_dist = HUGE;
  ymax = window.north;
  ymin = window.south;
  xmax = window.east;
  xmin = window.west;
  dig_P_read_line(map,abs(arc),&p);
  for (i=1; i < p->n_points; i++)
   {
    x1 = p->x[i-1];
    y1 = p->y[i-1];
    x2 = p->x[i];
    y2 = p->y[i];
    clip (&accept,&x1,&y1,&x2,&y2,xmin,xmax,ymin,ymax);
    if (accept)
     {
      pt_on_seg(&seg_ind,&seg_type,x1,y1,x2,y2,x_pt,y_pt,&x,&y,&dist);    
      seg_found = 1;
      if (dist < *shortest_dist)
       {
        *shortest_dist = dist;
        *near_seg = i;
       }
     }
   }
  return(seg_found);
 }
