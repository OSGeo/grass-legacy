/* M:  This function will assign the values "n", "e", "indicator",           */
/*     and "node" to the structure: "terminal".                              */
/* Function "M" allows the user to select either one of the end points of    */
/* the terminal segment as the terminal point.                               */
#include "distance.h"
int
M(next_letter,previous_letter,map,p)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  char DSLE_flag;
  extern int term();
  extern int valid_pt();
  extern int erase_pp();
  extern int erase_lm();
  extern int erase_o_g();
  extern int dr_me_pr_ex();
  extern int draw_SEGMENT();
  extern int draw_ARC();
  extern int draw_seg();

#ifdef DEBUG
fprintf(stderr,"M\n");
#endif DEBUG
  sprintf(line1,"Left Button:   Terminal point is \"orange\" plus.                                ");
  sprintf(line2,"Middle Button: Abort.                                                          ");
  sprintf(line3,"Right Button:  Terminal point is \"green\" plus.                                 ");
  if (arc_t_info.count > 1)
   {
    sprintf(line4,"|  Initial Point  |  Node Point     | Distance from Initial                   |"); 
    sprintf(line5,"|  \"red\" plus     |  \"violet\" plus  | to Node Point                           |");
    sprintf(line6,"| N %13.2f | N %13.2f | %14.3f meters (distance by arcs)|",initial.n,last_m_pt.n,last_m_pt.arc_dist_i_lm);
    sprintf(line7,"| E %13.2f | E %13.2f | %14.3f meters (shortest dist.)  |",initial.e,last_m_pt.e,last_m_pt.dir_dist_i_lm);                 
    strcpy(line8,"                                                                               ");
   }
  else
   {
    strcpy(line4,"                                                                               ");
    strcpy(line5,"                                                                               ");
    strcpy(line6,"                                                                               ");
    strcpy(line7,"                                                                               ");
    strcpy(line8,"                                                                               ");
   }
  term(line1,line2,line3,line4,line5,line6,line7,line8);
  dig_P_read_line(map,(abs(terminal.arc)),&p);
  R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
  if (button == 1)
   {
/* Must restrict which point on segment to choose if initial arc and         */
/* terminal arc are the same.                                                */
    if (arc_t_info.count==1)
     {
/* "arc_t_info.count" equals 1.                                              */
/* If "arc_t_info.count" equals 1 then initial arc and terminal arc are      */
/* the same.                                                                 */
      if ((initial.arc==terminal.arc)&&(terminal.segment==initial.segment))
       {
        if (valid_pt(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)])==1)
         {
/* Point (north,east) is valid.                                              */
          *previous_letter = 'M';
          *next_letter = 'Y';
/* Set values for "terminal" structure.                                      */
          terminal.n = p->y[(terminal.segment-1)];
          terminal.e = p->x[(terminal.segment-1)];
/* Terminal point is at beginning point of segment.                          */
/* Determine "indicator" and "node" for terminal point.                      */
          if ( (terminal.n == map->Node[map->Line[(abs(terminal.arc))].N1].y) && 
               (terminal.e == map->Node[map->Line[(abs(terminal.arc))].N1].x)   )
           {
/* "point" is terminal point and node N1.                                    */
            terminal.indicator = '6';
            terminal.node = map->Line[(abs(terminal.arc))].N1;
           }
          else
           {
/* "point" is terminal point and the beginning point of the terminal segment */
/* but is NOT a node point.                                                  */
            terminal.indicator = '8';
            terminal.node = 0;
           }
/* Set "arc_t_info.addr_last_arc" value.                                     */
         arc_t_info.addr_last_arc = arc_t_info.addr_first_arc + (arc_t_info.count-1);
/* Assign last arc value to arc_t_table.                                     */
         arc_t_info.addr_last_arc->arc = terminal.arc;
/* Erase "orange" and "green" plusses for terminal segment.                  */
          erase_o_g(map,p,'T');
/* Erase present point plus ("white plus")  and all the adjoining segments   */
          erase_pp(map,p);
/* Erase last measured point plus ("violet plus")  and all the               */
/* adjoining segments to that point.                                         */
          if (arc_t_info.count>0)
            erase_lm(map,p);
/* Erase "yellow" segment.                                                   */
          draw_SEGMENT(map,p,terminal.arc,terminal.segment,BG_COLOR);
          if (arc_t_info.count==1)
           {
            if (((initial.indicator=='1')&&(initial.arc<0)) ||
                ((initial.indicator=='2')&&(initial.arc>0))   )
             {
/* If initial.indicator = 1 then initial arc must be going in a negative     */
/* direction.                                                                */
/* If initial.indicator = 2 then initial arc must be going in a positive     */
/* direction.                                                                */
              if (terminal.arc>0)
               {
/* Terminal arc is going in a positive direction.                            */
                initial.arc = terminal.arc; 
                initial.indicator='6';
                initial.segment = 1;
                initial.node = map->Line[(abs(initial.arc))].N1;
               }
              else
               {
/* Terminal arc is going in a negative direction.                            */
                initial.arc = terminal.arc; 
                initial.indicator='7';
                dig_P_read_line(map,(abs(initial.arc)),&p);
                initial.segment = (p->n_points-1);
                initial.node = map->Line[(abs(initial.arc))].N2;
               }
             }
           }
/* Draw the direct line distance line from initial to terminal point.        */
          draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,DD_COLOR);
          total_points = 0;
/* Draw "red" line to terminal point.                                        */
          DSLE_flag = 'D';
          dr_me_pr_ex(map,p,DSLE_flag);
/* Add 1 to "total_points" for last point (terminal point).                  */
          total_points += 1;
#ifdef DEBUG_M
fprintf(stderr,"M:  total_points =%d\n",total_points);
fprintf(stderr,"M:  arc_t_info.count=%d\n",arc_t_info.count);
fprintf(stderr,"M:  initial.n=%lf initial.e=%lf\n",initial.n,initial.e);
fprintf(stderr,"M:  terminal.n=%lf terminal.e=%lf\n",terminal.n,terminal.e);
#endif DEBUG_M
/* Draw "initial point" plus as "red".                                       */ 
          draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw a "violet" plus for the terminal point.                              */
          draw_PLUS(map,p,terminal.n,terminal.e,TP_COLOR);
          return(1);
         }
       }
      else
       {
/* Point (north,east) is valid.                                              */
        *previous_letter = 'M';
        *next_letter = 'Y';
/* Set values for "terminal" structure.                                      */
        terminal.n = p->y[(terminal.segment-1)];
        terminal.e = p->x[(terminal.segment-1)];
/* Terminal point is at beginning point of segment.                          */
/* Determine "indicator" and "node" for terminal point.                      */
        if ( (terminal.n == map->Node[map->Line[(abs(terminal.arc))].N1].y) && 
             (terminal.e == map->Node[map->Line[(abs(terminal.arc))].N1].x)   )
         {
/* "point" is terminal point and node N1.                                    */
          terminal.indicator = '6';
          terminal.node = map->Line[(abs(terminal.arc))].N1;
         }
        else
         {
/* "point" is terminal point and the beginning point of the terminal segment */
/* but is NOT a node point.                                                  */
          terminal.indicator = '8';
          terminal.node = 0;
         }
/* Set "arc_t_info.addr_last_arc" value.                                     */
       arc_t_info.addr_last_arc = arc_t_info.addr_first_arc + (arc_t_info.count-1);
/* Assign last arc value to arc_t_table.                                     */
       arc_t_info.addr_last_arc->arc = terminal.arc;
/* Erase "orange" and "green" plusses for terminal segment.                  */
        erase_o_g(map,p,'T');
/* Erase present point plus ("white plus")  and all the adjoining segments.  */
        erase_pp(map,p);
/* Erase last measured point plus ("violet plus")  and all the               */
/* adjoining segments to that point.                                         */
        if (arc_t_info.count>0)
          erase_lm(map,p);
/* Erase "yellow" segment.                                                   */
        draw_SEGMENT(map,p,terminal.arc,terminal.segment,BG_COLOR);
        if (arc_t_info.count==1)
         {
          if (((initial.indicator=='1')&&(initial.arc<0)) ||
              ((initial.indicator=='2')&&(initial.arc>0))   )
           {
/* If initial.indicator = 1 then initial arc must be going in a negative     */
/* direction.                                                                */
/* If initial.indicator = 2 then initial arc must be going in a positive     */
/* direction.                                                                */
            if (terminal.arc>0)
             {
/* Terminal arc is going in a positive direction.                            */
              initial.arc = terminal.arc; 
              initial.indicator='6';
              initial.segment = 1;
              initial.node = map->Line[(abs(initial.arc))].N1;
             }
            else
             {
/* Terminal arc is going in a negative direction.                            */
              initial.arc = terminal.arc; 
              initial.indicator='7';
              dig_P_read_line(map,(abs(initial.arc)),&p);
              initial.segment = (p->n_points-1);
              initial.node = map->Line[(abs(initial.arc))].N2;
             }
           }
         }
/* Draw the direct line distance line from initial to terminal point.        */
        draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,DD_COLOR);
        total_points = 0;
/* Draw "red" line to terminal point.                                        */
        DSLE_flag = 'D';
        dr_me_pr_ex(map,p,DSLE_flag);
/* Add 1 to "total_points" for last point (terminal point).                  */
        total_points += 1;
/* Draw "initial point" plus as "red".                                       */ 
        draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw a "violet" plus for the terminal point.                              */
        draw_PLUS(map,p,terminal.n,terminal.e,TP_COLOR);
        return(1);
       }
     }
    else
     {
/* "arc_t_info.count" is greater than 1.                                     */
      *previous_letter = 'M';
      *next_letter = 'Y';
/* Set values for "terminal" structure.                                      */
      terminal.n = p->y[(terminal.segment-1)];
      terminal.e = p->x[(terminal.segment-1)];
/* Terminal point is at beginning point of segment.                          */
/* Determine "indicator" and "node" for terminal point.                      */
      if ( (terminal.n == map->Node[map->Line[(abs(terminal.arc))].N1].y) && 
           (terminal.e == map->Node[map->Line[(abs(terminal.arc))].N1].x)   )
       {
/* "point" is terminal point and node N1.                                    */
        terminal.indicator = '6';
        terminal.node = map->Line[(abs(terminal.arc))].N1;
       }
      else
       {
/* "point" is terminal point and the beginning point of the terminal segment */
/* but is NOT a node point.                                                  */
        terminal.indicator = '8';
        terminal.node = 0;
       }
/* Set "arc_t_info.addr_last_arc" value.                                     */
     arc_t_info.addr_last_arc = arc_t_info.addr_first_arc + (arc_t_info.count-1);
/* Assign last arc value to arc_t_table.                                     */
     arc_t_info.addr_last_arc->arc = terminal.arc;
/* Erase "orange" and "green" plusses for terminal segment.                  */
      erase_o_g(map,p,'T');
/* Erase present point plus ("white plus")  and all the adjoining segments.  */
      erase_pp(map,p);
/* Erase last measured point plus ("violet plus")  and all the               */
/* adjoining segments to that point.                                         */
      if (arc_t_info.count>0)
        erase_lm(map,p);
/* Erase "yellow" segment.                                                   */
      draw_SEGMENT(map,p,terminal.arc,terminal.segment,BG_COLOR);
      if (arc_t_info.count==1)
       {
        if (((initial.indicator=='1')&&(initial.arc<0)) ||
            ((initial.indicator=='2')&&(initial.arc>0))   )
         {
/* If initial.indicator = 1 then initial arc must be going in a negative     */
/* direction.                                                                */
/* If initial.indicator = 2 then initial arc must be going in a positive     */
/* direction.                                                                */
          if (terminal.arc>0)
           {
/* Terminal arc is going in a positive direction.                            */
            initial.arc = terminal.arc; 
            initial.indicator='6';
            initial.segment = 1;
            initial.node = map->Line[(abs(initial.arc))].N1;
           }
          else
           {
/* Terminal arc is going in a negative direction.                            */
            initial.arc = terminal.arc; 
            initial.indicator='7';
            dig_P_read_line(map,(abs(initial.arc)),&p);
            initial.segment = (p->n_points-1);
            initial.node = map->Line[(abs(initial.arc))].N2;
           }
         }
       }
      draw_ARC(map,p,terminal.arc,(int)1,IA_COLOR1,IA_COLOR2);
/* Draw the direct line distance line from initial to terminal point.        */
      draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,DD_COLOR);
      total_points = 0;
/* Draw "red" line to terminal point.                                        */
      DSLE_flag = 'D';
      dr_me_pr_ex(map,p,DSLE_flag);
/* Add 1 to "total_points" for last point (terminal point).                  */
      total_points += 1;
/* Draw "initial point" plus as "red".                                       */ 
      draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw a "violet" plus for the terminal point.                              */
      draw_PLUS(map,p,terminal.n,terminal.e,TP_COLOR);
      return(1);
     }
   }
  else
   {
    if (button == 2)
     {
      *previous_letter = 'M';
      *next_letter = 'Z';
      return(1);
     }
    else
     {
      if (button == 3)
       {
        dig_P_read_line(map,(abs(terminal.arc)),&p);
/* Must restrict which point on segment to choose if initial arc and         */
/* terminal arc are the same.                                                */
        if (arc_t_info.count==1)
         {
/* "arc_t_info.count" equals 1.                                              */
/* If "arc_t_info.count" equals 1 then initial arc and terminal arc are      */
/* the same.                                                                 */
          if (valid_pt(map,p,p->y[terminal.segment],p->x[terminal.segment])==1)
           {
/* Point (north,east) is valid.                                              */
/* Terminal point is at ending point of segment.                             */
            *previous_letter = 'M';
            *next_letter = 'Y';
/* Set values for "terminal" structure.                                      */
            terminal.n = p->y[terminal.segment];
            terminal.e = p->x[terminal.segment];
/* Determine "indicator" and "node" for terminal point.                      */
            if ( (terminal.n == map->Node[map->Line[(abs(terminal.arc))].N2].y) && 
                 (terminal.e == map->Node[map->Line[(abs(terminal.arc))].N2].x)   )
             {
/* "point" is terminal point and node N2.                                    */
              terminal.indicator = '7';
              terminal.node = map->Line[(abs(terminal.arc))].N2;
             }
            else
             {
/* "point" is terminal point and the ending point of the terminal segment    */
/* but is NOT a node point.                                                  */
              terminal.indicator = '9';
              terminal.node = 0;
             }
/* Set "arc_t_info.addr_last_arc" value.                                     */
            arc_t_info.addr_last_arc = arc_t_info.addr_first_arc + (arc_t_info.count-1);
/* Assign last arc value to arc_t_table.                                     */
            arc_t_info.addr_last_arc->arc = terminal.arc;
/* Erase "orange" and "green" plusses for terminal segment.                  */
            erase_o_g(map,p,'T');
/* Erase present point plus ("white plus")  and all the adjoining segments.  */
            erase_pp(map,p);
/* Erase last measured point plus ("violet plus")  and all the               */
/* adjoining segments to that point.                                         */
            if (arc_t_info.count>0)
              erase_lm(map,p);
/* Draw the terminal arc with the initial arc colors.                        */
            draw_ARC(map,p,terminal.arc,(int)1,IA_COLOR1,IA_COLOR2);
/* Draw the direct line distance line from initial to terminal point.        */
            draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,DD_COLOR);
            total_points = 0;
/* Draw "red" line to terminal point.                                        */
            DSLE_flag = 'D';
            dr_me_pr_ex(map,p,DSLE_flag);
/* Add 1 to "total_points" for last point (terminal point).                  */
            total_points += 1;
/* Draw "initial point" plus as "red".                                       */ 
            draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw a "violet" plus for the terminal point.                              */
            draw_PLUS(map,p,terminal.n,terminal.e,TP_COLOR);
            return(1);
           }
         }
        else
         {
          *previous_letter = 'M';
          *next_letter = 'Y';
/* Set values for "terminal" structure.                                      */
          terminal.n = p->y[terminal.segment];
          terminal.e = p->x[terminal.segment];
/* Determine "indicator" and "node" for terminal point.                      */
          if ( (terminal.n == map->Node[map->Line[(abs(terminal.arc))].N2].y) && 
               (terminal.e == map->Node[map->Line[(abs(terminal.arc))].N2].x)   )
           {
/* "point" is terminal point and node N2.                                    */
            terminal.indicator = '7';
            terminal.node = map->Line[(abs(terminal.arc))].N2;
           }
          else
           {
/* "point" is terminal point and the ending point of the terminal segment    */
/* but is NOT a node point.                                                  */
            terminal.indicator = '9';
            terminal.node = 0;
           }
/* Set "arc_t_info.addr_last_arc" value.                                     */
          arc_t_info.addr_last_arc = arc_t_info.addr_first_arc + (arc_t_info.count-1);
/* Assign last arc value to arc_t_table.                                     */
          arc_t_info.addr_last_arc->arc = terminal.arc;
/* Erase "orange" and "green" plusses for terminal segment.                  */
          erase_o_g(map,p,'T');
/* Erase present point plus ("white plus")  and all the adjoining segments.  */
          erase_pp(map,p);
/* Erase last measured point plus ("violet plus")  and all the               */
/* adjoining segments to that point.                                         */
          if (arc_t_info.count>0)
            erase_lm(map,p);
/* Draw the terminal arc with the initial arc colors.                        */
          draw_ARC(map,p,terminal.arc,(int)1,IA_COLOR1,IA_COLOR2);
/* Draw the direct line distance line from initial to terminal point.        */
          draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,DD_COLOR);
          total_points = 0;
/* Draw "red" line to terminal point.                                        */
          DSLE_flag = 'D';
          dr_me_pr_ex(map,p,DSLE_flag);
/* Add 1 to "total_points" for last point (terminal point).                  */
          total_points += 1;
/* Draw "initial point" plus as "red".                                       */ 
          draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw a "violet" plus for the terminal point.                              */
          draw_PLUS(map,p,terminal.n,terminal.e,TP_COLOR);
          return(1);
         }
       }
     }
   }
 }
