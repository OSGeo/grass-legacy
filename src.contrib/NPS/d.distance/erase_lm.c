#include "distance.h"
int
erase_lm(map,p)
 struct Map_info *map;
 struct line_pnts *p;
 {
  int i;

  dig_P_read_line(map,(abs(last_m_pt.arc)),&p);
/* Erase "last_m_pt" plus */
  draw_PLUS(map,p,last_m_pt.n,last_m_pt.e,BG_COLOR);
/* "last_m_pt point" is a beginning node point of a line */
/* If at first or last segment of "last_m_pt.arc" then redraw adjoining arcs */
/* "last_m_pt point" is both terminal point and node (N1) or node (N2) or */
/* "last_m_pt point" is simply node (N1) or node (N2) */
  if ( last_m_pt.segment==1||last_m_pt.segment==(p->n_points-1) )
   {
    for (i=0; i < map->Node[map->Line[abs(last_m_pt.arc)].N1].n_lines; i++)
     {
/* Redraw adjoining arcs with striping */
      if ( (abs(map->Node[map->Line[abs(last_m_pt.arc)].N1].lines[i]))!=(abs(last_m_pt.arc)) )
        draw_ARC(map,p,map->Node[map->Line[abs(last_m_pt.arc)].N1].lines[i],(int)1,IA_COLOR1,IA_COLOR2);
     }
    for (i=0; i < map->Node[map->Line[abs(last_m_pt.arc)].N2].n_lines; i++)
     {
/* Redraw adjoining arcs with striping */
      if ( (abs(map->Node[map->Line[abs(last_m_pt.arc)].N2].lines[i]))!=(abs(last_m_pt.arc)) )
        draw_ARC(map,p,map->Node[map->Line[abs(last_m_pt.arc)].N2].lines[i],(int)1,IA_COLOR1,IA_COLOR2);
     }
/* Redraw nodes for arcs adjoining to last_m_pt.arc */
    for (i=0; i < map->Node[map->Line[abs(last_m_pt.arc)].N1].n_lines; i++)
      draw_NODE(map,p,map->Node[map->Line[abs(last_m_pt.arc)].N1].lines[i],IN_COLOR);
/* Redraw nodes for arcs adjoining to last_m_pt.arc */
    for (i=0; i < map->Node[map->Line[abs(last_m_pt.arc)].N2].n_lines; i++)
      draw_NODE(map,p,map->Node[map->Line[abs(last_m_pt.arc)].N2].lines[i],IN_COLOR);
   }
  return(1);
 }
