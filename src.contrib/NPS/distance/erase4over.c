#include "distance.h"
int
erase4over(map,p)
 struct Map_info *map;
 struct line_pnts *p;
 {
  extern int erase_o_g();
  extern int erase_lm();
  extern int erase_pp();
  extern int draw_PLUS();
  extern int draw_seg();
#ifdef DEBUG
fprintf(stderr,"erase4over\n");
#endif DEBUG
  if ((initial.arc!=0)&&(initial.segment!=0))
   {
    erase_o_g(map,p,'I');
    if (  (initial.n != (double)0.0) && (initial.e != (double)0.0) )
     {
/* Erase initial point.                                                      */
      draw_PLUS(map,p,initial.n,initial.e,BG_COLOR);
      dig_P_read_line(map,(abs(initial.arc)),&p);
/* Erase initial segment.                                                    */
      if ((initial.n!=p->y[(initial.segment-1)])&&(initial.e!=p->x[(initial.segment-1)])&&
          (initial.n!=p->y[initial.segment])&&(initial.e!=p->x[initial.segment])           )
       {
/* Erase both partial segments of the initial segment.                       */
        draw_seg(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)],initial.n,initial.e,BG_COLOR);
        draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],BG_COLOR);
       } 
     }
   }
  if ((terminal.arc!=0)&&(terminal.segment!=0))
   {
    erase_o_g(map,p,'T');
    if (  (terminal.n != (double)0.0) && (terminal.e != (double)0.0) )
     {
/* Erase terminal point.                                                     */
      draw_PLUS(map,p,terminal.n,terminal.e,BG_COLOR);
/* Erase terminal segment.                                                   */
      dig_P_read_line(map,(abs(terminal.arc)),&p);
      if ((terminal.n!=p->y[(terminal.segment-1)])&&(terminal.e!=p->x[(terminal.segment-1)])&&
          (terminal.n!=p->y[terminal.segment])&&(terminal.e!=p->x[terminal.segment])           )
       {
/* Erase both partial segments of the terminal segment.                      */
        draw_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,BG_COLOR);
        draw_seg(map,p,terminal.n,terminal.e,p->y[terminal.segment],p->x[terminal.segment],BG_COLOR);
       } 
     }
   }
  if ((present.arc!=0)&&(present.segment!=0))
    erase_pp(map,p);
  if ((last_m_pt.arc!=0)&&(last_m_pt.segment!=0))
    erase_lm(map,p);
#ifdef DEBUG_e4o
fprintf(stderr,"erase4over:  initial.n=%lf initial.e=%lf\n",initial.n,initial.e);
fprintf(stderr,"erase4over:  arc_t_info.count=%d\n",arc_t_info.count);
#endif DEBUG_e4o
  if (arc_t_info.addr_last_arc != NULL)
   {
/* Erase initial and terminal point plusses.                                 */
/* and the direct "yellow" line between these points.                        */
    draw_PLUS(map,p,initial.n,initial.e,BG_COLOR);
    draw_PLUS(map,p,terminal.n,terminal.e,BG_COLOR);
    draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,BG_COLOR);
   }
 }
