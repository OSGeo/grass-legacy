#include "distance.h"
int
redraw_init(map,p,DSLE_flag)
 struct Map_info *map;
 struct line_pnts *p;
 {
  int i;

  if (abs(initial.arc)!=abs(terminal.arc))
     return(0);
  if (initial.indicator=='5')
   {
/* "point" is a mid-point of a segment as well as the initial point */
/* "point" is NOT an end point of a segment and is NOT a node */
/* Read initial arc */
    dig_P_read_line(map,abs(initial.arc),&p);
    if (initial.arc > 0)
     {
/* Initial arc is going in a positive direction */
/* Erase partial segment */
      draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],BG_COLOR);
/* Draw partial segment in "red" */
      draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],MA_COLOR);
      for (i=(initial.segment+1); i < p->n_points; i++)
       {
/* Erase segment */
        draw_SEGMENT(map,p,initial.arc,i,BG_COLOR);
/* Draw segment "red" segment */
        draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
       }
     }
    else
     {
/* Initial arc is going in a negative direction */
/* Erase partial segment */
      draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],BG_COLOR);
/* Draw "red" partial segment */
      draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],MA_COLOR);
      for (i=(initial.segment-1); i > 0; i--)
       {
/* Erase segment */
        draw_SEGMENT(map,p,initial.arc,i,BG_COLOR);
/* Draw "red" segment */
        draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
       }
     }
/* Redraw two end nodes for initial.arc */
    draw_NODE(map,p,map->Line[abs(initial.arc)].N1,IN_COLOR);
    draw_NODE(map,p,map->Line[abs(initial.arc)].N2,IN_COLOR);
   }
  else
   {
    if (initial.indicator=='3')
     {
/* "point" is the initial point as well as the beginning */
/* point of a segment but NOT a node point of an arc */
/* Read initial arc */
      dig_P_read_line(map,(abs(initial.arc)),&p);
      if (initial.arc>0)
       {
/* Initial arc is going in a positive direction */
        for (i=initial.segment; i < p->n_points; i++) 
         {
/* Erase segment */
          draw_SEGMENT(map,p,initial.arc,i,BG_COLOR);
/* Draw "red" segment */
          draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
         }
       }
      else
       {
/* Initial arc is going in a negative direction */
        for (i=(initial.segment-1); i > 0; i--)
         {
/* Erase segment */
          draw_SEGMENT(map,p,initial.arc,i,BG_COLOR);
/* Draw "red" segment */
          draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
         }
       }
/* Redraw two end nodes for initial.arc */
      draw_NODE(map,p,map->Line[abs(initial.arc)].N1,IN_COLOR);
      draw_NODE(map,p,map->Line[abs(initial.arc)].N2,IN_COLOR);
     }
    else
     {
      if (initial.indicator=='4')
       {
/* "point" is the initial point as well as the ending */
/* point of a segment but NOT a node point of an arc */
/* Read initial arc */
        dig_P_read_line(map,(abs(initial.arc)),&p);
        if (initial.arc>0)
         {
/* Initial arc is going in a positive direction */
          for (i=(initial.segment+1); i < p->n_points; i++) 
           {
/* Erase segment */
            draw_SEGMENT(map,p,initial.arc,i,BG_COLOR);
/* Draw "red" segment */
            draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
           }
         }
        else
         {
/* Initial arc is going in a negative direction */
          for (i=initial.segment; i > 0; i--)
           {
/* Erase segment */
            draw_SEGMENT(map,p,initial.arc,i,BG_COLOR);
/* Draw "red" segment */
            draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
           }
         }
/* Redraw two end nodes for initial.arc */
        draw_NODE(map,p,map->Line[abs(initial.arc)].N1,IN_COLOR);
        draw_NODE(map,p,map->Line[abs(initial.arc)].N2,IN_COLOR);
       }
      else
       {
        if ((initial.indicator=='1')||(initial.indicator=='2'))
         {
          if ( ((initial.indicator=='1')&&(initial.arc>0)) ||
               ((initial.indicator=='2')&&(initial.arc<0))   )
           {
/* "point" is a beginning or ending node point of a line */
/* "point" is both initial point and node (N1) or node (N2) */
/* If "point" is node N1 then the arc must be positive */
/* If "point" is node N2 then the arc must be negative */
            dig_P_read_line(map,(abs(initial.arc)),&p);
/* Erase arc */
            draw_ARC(map,p,initial.arc,(int)0,BG_COLOR,BG_COLOR);
/* Draw "red" measured line */
            draw_ARC(map,p,initial.arc,(int)0,MA_COLOR,MA_COLOR);
           }
/* Redraw two end nodes for initial.arc */
          draw_NODE(map,p,map->Line[abs(initial.arc)].N1,IN_COLOR);
          draw_NODE(map,p,map->Line[abs(initial.arc)].N2,IN_COLOR);
         }
       }
     }
   }
  return(1);
 }
