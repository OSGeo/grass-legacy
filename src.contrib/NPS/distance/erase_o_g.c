#include "distance.h"
int
erase_o_g(map,p,i_t_flag)
 struct Map_info *map;
 struct line_pnts *p;
 char i_t_flag;
 {
  int i;
  int arc;
  int segment;
  int return_status;

  if (i_t_flag == 'I')
   {
    arc = initial.arc;
    segment = initial.segment;
   }
  else
   {
    if (i_t_flag == 'T')
     {
      arc = terminal.arc;
      segment = terminal.segment;
     }
   }
  return_status=dig_P_read_line(map,(abs(arc)),&p);
/* Erase "orange" plus */
  draw_PLUS(map,p,p->y[(segment-1)],p->x[(segment-1)],BG_COLOR);
  return_status=dig_P_read_line(map,(abs(arc)),&p);
/* Erase "green" plus */ 
  draw_PLUS(map,p,p->y[segment],p->x[segment],BG_COLOR);
/* If at first or last segment of arc then redraw the  */
/* adjoining segments to node N1 and N2 of arc */
  if ( (segment==1) || (segment==(p->n_points-1)) )
   {
    if ( (segment==1) || (p->n_points==2) ) 
     {
      if ( map->Node[map->Line[abs(arc)].N1].alive==1 ) 
       {
/* "present point" is initial or terminal point and node (N1) */
        for (i=0; i < map->Node[map->Line[abs(arc)].N1].n_lines; i++)
         {
/* Redraw adjoining segments to node N1 and N2 of arc  */
          if ( (abs(map->Node[map->Line[abs(arc)].N1].lines[i]))!=(abs(arc)) )
           {
            return_status=dig_P_read_line(map,abs(map->Node[map->Line[abs(arc)].N1].lines[i]),&p);
            if (map->Node[map->Line[abs(arc)].N1].lines[i]>0)
             {
              draw_SEGMENT(map,p,map->Node[map->Line[abs(arc)].N1].lines[i],1,IA_COLOR1);
             }
            else
             {
              if ( ((p->n_points-1)%2) != 0)
               {
                draw_SEGMENT(map,p,map->Node[map->Line[abs(arc)].N1].lines[i],(p->n_points-1),IA_COLOR1);
               }
              else
               {
                draw_SEGMENT(map,p,map->Node[map->Line[abs(arc)].N1].lines[i],(p->n_points-1),IA_COLOR2);
               }
             }
           }
         }
       } 
     }
    if ( (segment==(p->n_points-1)) || (p->n_points==2) )
     {
      if ( map->Node[map->Line[abs(arc)].N2].alive==1 ) 
       {
/* "present point" is initial or terminal point and node (N2) */
        for (i=0; i < map->Node[map->Line[abs(arc)].N2].n_lines; i++)
         {
/* Redraw adjoining segments to beginning point of segment and initial point  */
/* or terminal point. */
          if ( (abs(map->Node[map->Line[abs(arc)].N2].lines[i]))!=(abs(arc)) )
           {
            return_status=dig_P_read_line(map,abs(map->Node[map->Line[abs(arc)].N2].lines[i]),&p);
            if (map->Node[map->Line[abs(arc)].N2].lines[i]>0)
             {
              draw_SEGMENT(map,p,map->Node[map->Line[abs(arc)].N1].lines[i],1,IA_COLOR1);
             }
            else
             {
              if ( ((p->n_points-1)%2) != 0)
               {
                draw_SEGMENT(map,p,map->Node[map->Line[abs(arc)].N2].lines[i],(p->n_points-1),IA_COLOR1);
               }
              else
               {
                draw_SEGMENT(map,p,map->Node[map->Line[abs(arc)].N2].lines[i],(p->n_points-1),IA_COLOR2);
               }
             }
           }
         }
       }
     }
    if ( (segment==1) || (p->n_points==2) )
     {
      if ( map->Node[map->Line[abs(arc)].N1].alive==1 ) 
       {
/* Redraw nodes for arcs adjoining to arc */
        for (i=0; i < map->Node[map->Line[abs(arc)].N1].n_lines; i++)
         {
/* Redraw nodes for the adjoining arcs */
          draw_NODE(map,p,map->Node[map->Line[abs(arc)].N1].lines[i],IN_COLOR);
         }
       }
     }
    if ( (segment==(p->n_points-1)) || (p->n_points==2) )
     {
      if ( map->Node[map->Line[abs(arc)].N2].alive==1 ) 
       {
/* Redraw nodes for arcs adjoining to arc */
        for (i=0; i < map->Node[map->Line[abs(arc)].N2].n_lines; i++)
         {
/* Redraw nodes for the adjoining arcs */
          draw_NODE(map,p,map->Node[map->Line[abs(arc)].N2].lines[i],IN_COLOR);
         }
       }
     }
   }
  return(1);
 }
