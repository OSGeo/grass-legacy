#include "distance.h"
int
erase_pp(map,p)
 struct Map_info *map;
 struct line_pnts *p;
 {
  int i;

  dig_P_read_line(map,(abs(present.arc)),&p);
/* Erase "present" plus */
  draw_PLUS(map,p,present.n,present.e,BG_COLOR);
/* "present point" is a beginning node point of a line */
/* If at first or last segment of "present.arc" then redraw adjoining arcs */
/* "present point" is both initial point and node (N1) or node (N2) or */
/* "present point" is simply node (N1) or node (N2) */
  if ( present.segment==1||present.segment==(p->n_points-1) )
   {
    for (i=0; i < map->Node[map->Line[abs(present.arc)].N1].n_lines; i++)
     {
/* Redraw adjoining arcs with striping */
      if ( (abs(map->Node[map->Line[abs(present.arc)].N1].lines[i]))!=(abs(present.arc)) )
        draw_ARC(map,p,map->Node[map->Line[abs(present.arc)].N1].lines[i],(int)1,IA_COLOR1,IA_COLOR2);
     }
    for (i=0; i < map->Node[map->Line[abs(present.arc)].N2].n_lines; i++)
     {
/* Redraw adjoining arcs with striping */
      if ( (abs(map->Node[map->Line[abs(present.arc)].N2].lines[i]))!=(abs(present.arc)) )
        draw_ARC(map,p,map->Node[map->Line[abs(present.arc)].N2].lines[i],(int)1,IA_COLOR1,IA_COLOR2);
     }
/* Redraw nodes for arcs adjoining to present.arc */
    for (i=0; i < map->Node[map->Line[abs(present.arc)].N1].n_lines; i++)
      draw_NODE(map,p,map->Node[map->Line[abs(present.arc)].N1].lines[i],IN_COLOR);
/* Redraw nodes for arcs adjoining to present.arc */
    for (i=0; i < map->Node[map->Line[abs(present.arc)].N2].n_lines; i++)
      draw_NODE(map,p,map->Node[map->Line[abs(present.arc)].N2].lines[i],IN_COLOR);
   }
  return(1);
 }
