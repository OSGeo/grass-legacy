#include "distance.h"
int
chosen_arc(map,p,chosen_arc)
 struct Map_info *map;
 struct line_pnts *p;
 int chosen_arc;
 {
  extern int draw_seg();
  int i;
  char color[19];
  int stripe;
  extern int draw_seg();
  extern int draw_SEGMENT();
  extern int draw_ARC();
  int violet_arc_pos;
  int arc_num;
  int present_node;
  int start_seg;
  char color1[19], color2[19];
  char color3[19], color4[19];

  dig_P_read_line(map,(abs(present.arc)),&p);
/* initial point */
  if (present.indicator=='5')
   {
/* "present point" is a mid-point of a segment as well as the initial point */
/* "present point" is NOT an end point of a segment and is NOT a node */
    if (chosen_arc > 0)
     {
      strcpy(color1,HL_COLOR1);
      strcpy(color2,HL_COLOR2);
     }
    else
     {
      strcpy(color1,IA_COLOR1);
      strcpy(color2,IA_COLOR2);
     }
/* Read present arc */
    dig_P_read_line(map,(abs(present.arc)),&p);
/* Erase present segment */
    draw_seg(map,p,p->y[(present.segment-1)],p->x[(present.segment-1)],p->y[present.segment],p->x[present.segment],BG_COLOR);
/* Draw partial segment of arc with color1 or color2 */
    if ( present.segment%2 != 0)
      draw_seg(map,p,present.n,present.e,p->y[present.segment],p->x[present.segment],color1);
    else
      draw_seg(map,p,present.n,present.e,p->y[present.segment],p->x[present.segment],color2);
    for (i=(present.segment+1); i < p->n_points; i++)
     {
/* Erase segment */
      draw_SEGMENT(map,p,present.arc,i,BG_COLOR);
  /* Draw segment */
      if ( (i%2) != 0)
        draw_SEGMENT(map,p,present.arc,i,color1);
      else
        draw_SEGMENT(map,p,present.arc,i,color2);
     }
    if (chosen_arc > 0)
     {
      strcpy(color1,IA_COLOR1);
      strcpy(color2,IA_COLOR2);
     }
    else
     {
      strcpy(color1,HL_COLOR1);
      strcpy(color2,HL_COLOR2);
     }
/* Draw partial segment of arc with color1 or color2 */
    if ( present.segment%2 != 0)
      draw_seg(map,p,present.n,present.e,p->y[(present.segment-1)],p->x[(present.segment-1)],color1);
    else
      draw_seg(map,p,present.n,present.e,p->y[(present.segment-1)],p->x[(present.segment-1)],color2);
    for (i=(present.segment-1); i > 0; i--)
     {
/* Erase segment */
      draw_SEGMENT(map,p,present.arc,i,BG_COLOR);
/* Draw segment */
      if ( (i%2) != 0)
        draw_SEGMENT(map,p,present.arc,i,color1);
      else
        draw_SEGMENT(map,p,present.arc,i,color2);
     }
/* Redraw two end nodes for present.arc */
    draw_NODE(map,p,map->Line[abs(present.arc)].N1,IN_COLOR);
    draw_NODE(map,p,map->Line[abs(present.arc)].N2,IN_COLOR);
/* Redraw "red" plus (which is the initial point) */
    draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
   }
  else
   {
    if (present.indicator=='3')
     {
/* "present point" is the initial point as well as the beginning */
/* point of a segment but NOT a node point of an arc */
      if (chosen_arc>0)
       {
        strcpy(color1,HL_COLOR1);
        strcpy(color2,HL_COLOR2);
       }
      else
       {
        strcpy(color1,IA_COLOR1);
        strcpy(color2,IA_COLOR2);
       }
/* Read present arc */
      dig_P_read_line(map,(abs(present.arc)),&p);
/* draw part of arc with color1 */
      for (i=present.segment; i < p->n_points; i++) 
       {
/* Erase segment */
        draw_SEGMENT(map,p,present.arc,i,BG_COLOR);
/* Draw segment */
        if ( present.segment%2 != 0)
         {
          if (i%2 != 0)
            draw_SEGMENT(map,p,present.arc,i,color1);
          else
            draw_SEGMENT(map,p,present.arc,i,color2);
         }
        else
         {
          if (i%2 != 0)
            draw_SEGMENT(map,p,present.arc,i,color1);
          else
            draw_SEGMENT(map,p,present.arc,i,color2);
         }
       }
      if (chosen_arc > 0)
       {
        strcpy(color1,IA_COLOR1);
        strcpy(color2,IA_COLOR2);
       }
      else
       {
        strcpy(color1,HL_COLOR1);
        strcpy(color2,HL_COLOR2);
       }
/* draw part of arc with color2 */
      for (i=(present.segment-1); i > 0; i--)
       {
/* Erase segment */
        draw_SEGMENT(map,p,present.arc,i,BG_COLOR);
/* Draw segment */
        if ( present.segment%2 != 0)
         {
          if (i%2 != 0)
            draw_SEGMENT(map,p,present.arc,i,color1);
          else
            draw_SEGMENT(map,p,present.arc,i,color2);
         }
        else
         {
          if (i%2 != 0)
            draw_SEGMENT(map,p,present.arc,i,color1);
          else
            draw_SEGMENT(map,p,present.arc,i,color2);
         }
       }
/* Redraw two end nodes for present.arc */
      draw_NODE(map,p,map->Line[abs(present.arc)].N1,IN_COLOR);
      draw_NODE(map,p,map->Line[abs(present.arc)].N2,IN_COLOR);
/* Redraw "red" plus (which is the initial point) */
      draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
     }
    else
     {
      if (present.indicator=='4')
       {
/* "present point" is the initial point as well as the ending */
/* point of a segment but NOT a node point of an arc */
        if (chosen_arc > 0)
         {
          strcpy(color1,HL_COLOR1);
          strcpy(color2,HL_COLOR2);
         }
        else
         {
          strcpy(color1,IA_COLOR1);
          strcpy(color2,IA_COLOR2);
         }
/* Read present arc */
        dig_P_read_line(map,(abs(present.arc)),&p);
/* draw part of arc with color1 */
        for (i=(present.segment+1); i < p->n_points; i++) 
         {
/* Erase segment */
          draw_SEGMENT(map,p,present.arc,i,BG_COLOR);
/* Draw segment */
          if ( present.segment%2 != 0)
           {
            if (i%2 != 0)
              draw_SEGMENT(map,p,present.arc,i,color1);
            else
              draw_SEGMENT(map,p,present.arc,i,color2);
           }
          else
           {
            if (i%2 != 0)
              draw_SEGMENT(map,p,present.arc,i,color1);
            else
              draw_SEGMENT(map,p,present.arc,i,color2);
           }
         }
        if (chosen_arc > 0)
         {
          strcpy(color1,IA_COLOR1);
          strcpy(color2,IA_COLOR2);
         }
        else
         {
          strcpy(color1,HL_COLOR1);
          strcpy(color2,HL_COLOR2);
         }
/* draw part of arc with color2 */
        for (i=present.segment; i > 0; i--)
         {
/* Erase segment */
          draw_SEGMENT(map,p,present.arc,i,BG_COLOR);
/* Draw segment */
          if ( present.segment%2 != 0)
           {
            if (i%2 != 0)
              draw_SEGMENT(map,p,present.arc,i,color1);
            else
              draw_SEGMENT(map,p,present.arc,i,color2);
           }
          else
           {
            if (i%2 != 0)
              draw_SEGMENT(map,p,present.arc,i,color1);
            else
              draw_SEGMENT(map,p,present.arc,i,color2);
           }
         }
/* Redraw two end nodes for present.arc */
        draw_NODE(map,p,map->Line[abs(present.arc)].N1,IN_COLOR);
        draw_NODE(map,p,map->Line[abs(present.arc)].N2,IN_COLOR);
/* Redraw "red" plus (which is the initial point) */
        draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
       }
      else
       {
        if ((present.indicator=='1')||(present.indicator=='2')||
            (present.indicator=='a')||(present.indicator=='b')  )

         {
          dig_P_read_line(map,(abs(present.arc)),&p);
/* can insert here "erase_pp" later.... */
          if (map->Node[present.node].alive==1)
           {
            for (i=0; i < map->Node[present.node].n_lines; i++)
             {
              if ((map->Node[present.node].lines[i] ==   chosen_arc) || 
                  (map->Node[present.node].lines[i] == (-chosen_arc))  )
               {
                violet_arc_pos = i;
                strcpy(color1,HL_COLOR1);
                strcpy(color2,HL_COLOR2);
               }
              else
               {
                strcpy(color1,HL_COLOR1);
                strcpy(color2,HL_COLOR2);
               }
              if (map->Node[present.node].lines[i] < 0)
                arc_num = (-(map->Node[present.node].lines[i]));
              else
                arc_num = map->Node[present.node].lines[i];
/* Stripe the "violet" arc  */
              if (i == violet_arc_pos)
               {
/* Erase arc */
                draw_ARC(map,p,arc_num,(int)0,BG_COLOR,BG_COLOR);
/* Draw "violet/green" chosen arc */
                draw_ARC(map,p,arc_num,(int)1,color1,color2);
                redraw_adj(map,p,violet_arc_pos);
               }
             }
           }
/* Redraw two end nodes for present.arc */
          draw_NODE(map,p,map->Line[abs(present.arc)].N1,IN_COLOR);
          draw_NODE(map,p,map->Line[abs(present.arc)].N2,IN_COLOR);
/* Redraw "red" plus (which is the initial point) */
          draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
         }
       }
     }
   }
  return(1);
 }


int
redraw_adj(map,p,violet_arc_pos)
 struct Map_info *map;
 struct line_pnts *p;
 int violet_arc_pos;
 {
  int i;
  int arc_num;

  if (map->Node[present.node].alive==1)
   {
    dig_P_read_line(map,(abs(present.arc)),&p);
/* Redraw all arcs using IA_COLOR1 and IA_COLOR2 except the chosen */
/* violet arc. */
    for (i=0; i < map->Node[present.node].n_lines; i++)
     {
       if (i != violet_arc_pos)
        {
         arc_num = map->Node[present.node].lines[i];
         dig_P_read_line(map,(abs(arc_num)),&p);
         draw_ARC(map,p,arc_num,(int)1,IA_COLOR1,IA_COLOR2);
        }
     }
/* Redraw "yellow" node plusses */
    for (i=0; i < map->Node[present.node].n_lines; i++)
     {
      dig_P_read_line(map,abs(map->Node[present.node].lines[i]),&p);
      draw_NODE(map,p,map->Line[abs(map->Node[present.node].lines[i])].N1,IN_COLOR);
      draw_NODE(map,p,map->Line[abs(map->Node[present.node].lines[i])].N2,IN_COLOR);
     }
   }
  return(1);
 }
