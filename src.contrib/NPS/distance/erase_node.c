#include "distance.h"
int
erase_node(map,p,node_num)
 struct Map_info *map;
 struct line_pnts *p;
 int node_num;
 {
  int i;
  extern draw_PLUS();
  extern draw_SEGMENT();
  extern draw_NODE();

/* Erase node */
  draw_PLUS(map,p,map->Node[node_num].y,map->Node[node_num].x,BG_COLOR);
  if ( map->Node[node_num].alive==1 ) 
   {
    for (i=0; i < map->Node[node_num].n_lines; i++)
     {
      if (map->Node[node_num].lines[i]>0)
       {
        draw_SEGMENT(map,p,map->Node[node_num].lines[i],1,IA_COLOR1);
       }
      else
       {
        dig_P_read_line(map,abs(map->Node[node_num].lines[i]),&p);
        if ( ((p->n_points-1)%2) != 0)
         {
          draw_SEGMENT(map,p,map->Node[node_num].lines[i],(p->n_points-1),IA_COLOR1);
         }
        else
         {
          draw_SEGMENT(map,p,map->Node[node_num].lines[i],(p->n_points-1),IA_COLOR2);
         }
       }
     }
/* Redraw nodes for arcs adjoining to node point */
    for (i=0; i < map->Node[node_num].n_lines; i++)
     {
/* Redraw nodes for the adjoining arcs to the node point */
      draw_NODE(map,p,map->Line[abs(map->Node[node_num].lines[i])].N1,IN_COLOR);
      draw_NODE(map,p,map->Line[abs(map->Node[node_num].lines[i])].N2,IN_COLOR);
     }
   }
  return(1);
 }
