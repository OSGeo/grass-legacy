#include "distance.h"
int
choose_arc(map,p,hl_arc)
 struct Map_info *map;
 struct line_pnts *p;
 int *hl_arc;
 {
  extern int draw_seg();
  extern arc_is_dot();
  int i;
  int stripe;
  extern int draw_seg();
  extern int draw_SEGMENT();
  extern int draw_ARC();
  int violet_arc_pos;
  int arc_num;
  int present_node;
  int start_seg;
  char color1[19], color2[19];
  int search, search_pos, search_arc;

  dig_P_read_line(map,(abs(present.arc)),&p);
/* initial point */
  if (present.indicator=='5')
   {
/* assign "*hl_arc" to present.arc */
    present.arc = *hl_arc;
/* "present point" is a mid-point of a segment as well as the initial point */
/* "present point" is NOT an end point of a segment and is NOT a node */
    if (*hl_arc>0)
     {
      strcpy(color1,HL_COLOR1);
      strcpy(color2,HL_COLOR2);
     }
    else
     {
      strcpy(color1,HL_COLOR2);
      strcpy(color2,HL_COLOR1);
     }
/* Read present arc */
    dig_P_read_line(map,(abs(present.arc)),&p);
/* Erase present segment */
    draw_seg(map,p,p->y[(present.segment-1)],p->x[(present.segment-1)],p->y[present.segment],p->x[present.segment],BG_COLOR);
/* Draw part of arc with color1 */
    draw_seg(map,p,present.n,present.e,p->y[present.segment],p->x[present.segment],color1);
    for (i=(present.segment+1); i < p->n_points; i++)
     {
/* Erase segment */
      draw_SEGMENT(map,p,present.arc,i,BG_COLOR);
/* Draw segment */
      draw_SEGMENT(map,p,present.arc,i,color1);
     }
/* Draw part of arc with color2 */
    draw_seg(map,p,present.n,present.e,p->y[(present.segment-1)],p->x[(present.segment-1)],color2);
    for (i=(present.segment-1); i > 0; i--)
     {
/* Erase segment */
      draw_SEGMENT(map,p,present.arc,i,BG_COLOR);
/* Draw segment */
      draw_SEGMENT(map,p,present.arc,i,color2);
     }
/* Redraw two end nodes for present.arc */
    draw_NODE(map,p,map->Line[abs(present.arc)].N1,IN_COLOR);
    draw_NODE(map,p,map->Line[abs(present.arc)].N2,IN_COLOR);
/* Redraw "red" plus (which is the initial point) */
    draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* assign "*hl_arc" to present.arc */
    present.arc = *hl_arc;
/* Set next *hl_arc value */
    *hl_arc = -(*hl_arc);
   }
  else
   {
    if (present.indicator=='3')
     {
/* "present point" is the initial point as well as the beginning */
/* point of a segment but NOT a node point of an arc */
      if (*hl_arc>0)
       {
        strcpy(color1,HL_COLOR1);
        strcpy(color2,HL_COLOR2);
       }
      else
       {
        strcpy(color1,HL_COLOR2);
        strcpy(color2,HL_COLOR1);
       }
/* Read present arc */
      dig_P_read_line(map,(abs(present.arc)),&p);
/* draw part of arc with color1 */
      for (i=present.segment; i < p->n_points; i++) 
       {
/* Erase segment */
        draw_SEGMENT(map,p,present.arc,i,BG_COLOR);
/* Draw segment */
        draw_SEGMENT(map,p,present.arc,i,color1);
       }
/* draw part of arc with color2 */
      for (i=(present.segment-1); i > 0; i--)
       {
/* Erase segment */
        draw_SEGMENT(map,p,present.arc,i,BG_COLOR);
/* Draw segment */
        draw_SEGMENT(map,p,present.arc,i,color2);
       }
/* Redraw two end nodes for present.arc */
      draw_NODE(map,p,map->Line[abs(present.arc)].N1,IN_COLOR);
      draw_NODE(map,p,map->Line[abs(present.arc)].N2,IN_COLOR);
/* Redraw "red" plus (which is the initial point) */
      draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* assign "*hl_arc" to present.arc */
      present.arc = *hl_arc;
/* Set new *hl_arc value */
      *hl_arc = -(*hl_arc);
     }
    else
     {
      if (present.indicator=='4')
       {
/* "present point" is the initial point as well as the ending */
/* point of a segment but NOT a node point of an arc */
        if (*hl_arc>0)
         {
          strcpy(color1,HL_COLOR1);
          strcpy(color2,HL_COLOR2);
         }
        else
         {
          strcpy(color1,HL_COLOR2);
          strcpy(color2,HL_COLOR1);
         }
/* Read present arc */
        dig_P_read_line(map,(abs(present.arc)),&p);
/* draw part of arc with color1 */
        for (i=(present.segment+1); i < p->n_points; i++) 
         {
/* Erase segment */
          draw_SEGMENT(map,p,present.arc,i,BG_COLOR);
/* Draw segment */
          draw_SEGMENT(map,p,present.arc,i,color1);
         }
/* draw part of arc with color2 */
        for (i=present.segment; i > 0; i--)
         {
/* Erase segment */
          draw_SEGMENT(map,p,present.arc,i,BG_COLOR);
/* Draw segment */
          draw_SEGMENT(map,p,present.arc,i,color2);
         }
/* Redraw two end nodes for present.arc */
        draw_NODE(map,p,map->Line[abs(present.arc)].N1,IN_COLOR);
        draw_NODE(map,p,map->Line[abs(present.arc)].N2,IN_COLOR);
/* Redraw "red" plus (which is the initial point) */
        draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* assign "*hl_arc" to present.arc */
        present.arc = *hl_arc;
/* Set new *hl_arc value */
        *hl_arc = -(*hl_arc);
       }
      else
       {
        if ((present.indicator=='1')||(present.indicator=='2')||
            (present.indicator=='a')||(present.indicator=='b')  )
         {
/* "present point" is a beginning or ending node point of a line */
/* "present point" is both initial point and node (N1) or node (N2) or */
/* "present point" is simply node (N1) or (N2) */

/* Read present arc */
          dig_P_read_line(map,(abs(present.arc)),&p);

          if (map->Node[present.node].alive==1)
           {
/* Draw violet and green arcs */
          for (i=0; i < map->Node[present.node].n_lines; i++)
           {
            if ((map->Node[present.node].lines[i] ==   (*hl_arc)) ||  
                (map->Node[present.node].lines[i] == (-(*hl_arc)))   )
             {
              violet_arc_pos = i;
              strcpy(color1,HL_COLOR1);
              strcpy(color2,HL_COLOR2);
             }
            else
             {
              strcpy(color1,HL_COLOR2);
              strcpy(color2,HL_COLOR1);
             }
            if (map->Node[present.node].lines[i] < 0)
              arc_num = (-(map->Node[present.node].lines[i]));
            else
              arc_num = map->Node[present.node].lines[i];
/* Erase arc */
            draw_ARC(map,p,arc_num,(int)0,BG_COLOR,BG_COLOR);
/* Draw arc as either violet or green */
            if (i == violet_arc_pos)
              draw_ARC(map,p,arc_num,(int)0,color1,color2);
            else
              draw_ARC(map,p,arc_num,(int)0,color1,color2);
/* Redraw two end nodes for "arc_num" */
            draw_NODE(map,p,map->Line[abs(arc_num)].N1,IN_COLOR);
            draw_NODE(map,p,map->Line[abs(arc_num)].N2,IN_COLOR);
           }

           }

/* assign "*hl_arc" to present.arc */
          present.arc = *hl_arc;
          search = 1;
          search_arc = present.arc;
          search_pos = violet_arc_pos;
          while(search)
           {
              search_pos = search_pos + 1;
/* Set next *hl_arc value */
            if (search_pos == map->Node[present.node].n_lines)
             {
              search_pos = 0;
             }
            search_arc = map->Node[present.node].lines[search_pos];
            if (arc_is_dot(map,p,search_arc)!=1)
             {
              *hl_arc = search_arc;
              search = 0;
             }
           }
/* Draw last measured point plus as violet */
          if (arc_t_info.count > 1)
            draw_PLUS(map,p,last_m_pt.n,last_m_pt.e,LM_COLOR);
/* Draw next "present point" plus as "white" */ 
          draw_PLUS(map,p,present.n,present.e,PP_COLOR);
/* Redraw "red" plus (which is the initial point) */
          draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* assign present.segment for new "*hl_arc" (which is now "present.arc) */
          if (*hl_arc > 0)
           {
            present.segment = 1;
            if ((present.indicator=='1')||(present.indicator=='2'))
              present.indicator = '1';
            if ((present.indicator=='a')||(present.indicator=='b'))
              present.indicator = 'a';
           }
          else
           {
            dig_P_read_line(map,(abs(present.arc)),&p);
            present.segment = (p->n_points-1);  
            if ((present.indicator=='1'||present.indicator=='2'))
              present.indicator = '2';
            if ((present.indicator=='a'||present.indicator=='b'))
              present.indicator = 'b';
           }
         }
       }
     }
   }
  return(1);
 }
