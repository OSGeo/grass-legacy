/* "arc_in_WIND" verifies whether arc number "arc" is entirely within the    */
/* "WIND" area.  If "arc" is entirely within the "WIND" area then this       */
/* function returns a 1.  If "arc" is NOT entirely within the "WIND" area    */
/* this function will return a 0.                                            */
#include "distance.h"
int
arc_in_WIND(map,p,arc)
  struct Map_info *map;
  struct line_pnts *p;
  int arc;
 {
  register int i;
  int all_arc_in;

  all_arc_in = 0;
  dig_P_read_line(map,abs(arc),&p);
  for (i=0; i < p->n_points; i++)
   {
    if (p->y[i]<=window.north && p->y[i]>=window.south &&
        p->x[i]<=window.east  && p->x[i]>=window.west    )
     {
      all_arc_in = 1;
     }
    else
     {
      all_arc_in = 0;
      return(all_arc_in);
     }
   }
  return(all_arc_in);
 }
