/* N:  This function will allow the user to confirm whether the chosen point */
/* on the terminal segment is the terminal point that the user wants or not. */
#include "distance.h"
int
N(next_letter,previous_letter,map,p)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  char DSLE_flag;
  static char prev_letter_E_P;
  extern int term();
  extern int erase_pp();
  extern int erase_lm();
  extern int erase_o_g();
  extern int draw_SEGMENT();
  extern int draw_seg();
  extern int dr_me_pr_ex();
  extern int valid_pt();
  extern int draw_PLUS();
  extern int pt_in_WIND();

#ifdef DEBUG
fprintf(stderr,"N\n");
#endif DEBUG
  sprintf(line1,"Left Button:   Terminal point (\"red plus\") on \"yellow\" segment is correct.     ");
  sprintf(line2,"Middle Button: Abort.                                                          ");
  sprintf(line3,"Right Button:  Terminal point (\"red plus\") on \"yellow\" segment is NOT correct. ");
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
  if ((*previous_letter=='E')||(*previous_letter=='P'))
    prev_letter_E_P = *previous_letter;
  else
    *previous_letter = prev_letter_E_P;
#ifdef DEBUG_N
fprintf(stderr,"N:  *previous_letter=%c\n",*previous_letter);
fprintf(stderr,"N:  *next_letter=%c\n",*next_letter);
fprintf(stderr,"N:  prev_letter_E_P=%c\n",prev_letter_E_P);
#endif DEBUG_N
#ifdef DEBUG_N
fprintf(stderr,"N:  *previous_letter=%c\n",*previous_letter);
fprintf(stderr,"N:  *next_letter=%c\n",*next_letter);
#endif DEBUG_N
  if ((*previous_letter=='E')||(*previous_letter=='N'))
   {
#ifdef DEBUG_N
fprintf(stderr,"N:  redraw orange and green\n");
#endif DEBUG_N
     dig_P_read_line(map,(abs(terminal.arc)),&p);
     if ( (pt_in_WIND(p->y[(terminal.segment-1)],p->x[(terminal.segment-1)]))&&
          (pt_in_WIND(p->y[terminal.segment],p->x[terminal.segment]))          )
      {
/* Redraw the "orange" plus for the beginning point of segment.              */
       draw_PLUS(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],BS_COLOR);
/* Redraw the "green" plus for the ending point of segment.                  */
       draw_PLUS(map,p,p->y[terminal.segment],p->x[terminal.segment],ES_COLOR);
      }
    }
#ifdef DEBUG_N
fprintf(stderr,"N:  redraw red plus\n");
#endif DEBUG_N
/* Draw a "red" plus for the potential terminal point.                       */
  draw_PLUS(map,p,terminal.n,terminal.e,IP_COLOR);
  R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
#ifdef DEBUG_N
fprintf(stderr,"N:  button=%d\n",button);
#endif DEBUG_N
  if (button == 1)
   {
/* Set "arc_t_info.addr_last_arc" value.                                     */
    arc_t_info.addr_last_arc = arc_t_info.addr_first_arc + (arc_t_info.count-1);
/* Assign last arc value to arc_t_table.                                     */
    arc_t_info.addr_last_arc->arc = terminal.arc;
/* Erase "orange" and "green" plusses.                                       */
    if (*previous_letter=='E')
      erase_o_g(map,p,'T');
/* Erase present point plus ("white plus")  and all the adjoining segments.  */
    erase_pp(map,p);
/* Erase last measured point plus ("violet plus")  and all the               */
/* adjoining segments to that point.                                         */
    if (arc_t_info.count>0)
      erase_lm(map,p);
/* Erase terminal segment.                                                   */
    draw_SEGMENT(map,p,terminal.arc,terminal.segment,BG_COLOR);
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
/* Draw the direct line distance line from initial to terminal point.        */
    draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,DD_COLOR);
   total_points = 0;
/* Draw "red" line to terminal point.                                        */
    DSLE_flag = 'D';
    dr_me_pr_ex(map,p,DSLE_flag);
/* Add 1 to "total_points" for last point (terminal point).                  */
    total_points += 1;
#ifdef DEBUG_N
fprintf(stderr,"N:  total_points =%d\n",total_points);
fprintf(stderr,"N:  arc_t_info.count=%d\n",arc_t_info.count);
fprintf(stderr,"N:  initial.n=%lf initial.e=%lf\n",initial.n,initial.e);
fprintf(stderr,"N:  terminal.n=%lf terminal.e=%lf\n",terminal.n,terminal.e);
#endif DEBUG_N
/* Draw next "initial point" plus as "red".                                  */ 
    draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw a "violet" plus for the terminal point.                              */
    draw_PLUS(map,p,terminal.n,terminal.e,TP_COLOR);
    *previous_letter = 'N';
    *next_letter = 'Y';
    return(1);
   }
  else
   {
    if (button == 2)
     {
#ifdef DEBUG_N
fprintf(stderr,"N:  button=%d\n",button);
#endif DEBUG_N
/* "Undo" the "red" plus and before going to "Z".                            */
      dig_P_read_line(map,(abs(terminal.arc)),&p);
/* Draw a "black" plus over the "red" plus on the "yellow" segment.          */
      draw_PLUS(map,p,terminal.n,terminal.e,BG_COLOR);
/* Draw "yellow" segment.                                                    */
      if (arc_t_info.count==1)
       {
/* Determine if both orange and green plusses are valid or not.              */
        if ((abs(terminal.arc)==abs(initial.arc))&&(terminal.segment==initial.segment))
         {
          dig_P_read_line(map,(abs(initial.arc)),&p);
#ifdef DEBUG_N
fprintf(stderr,"N:  1:before valid_pt\n");
#endif DEBUG_N
          if ((valid_pt(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)])==1)
            &&(valid_pt(map,p,p->y[initial.segment],p->x[initial.segment])==1)       )
           {
            dig_P_read_line(map,(abs(terminal.arc)),&p);
/* Erase both partial segments of terminal segment.                           */
            draw_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,BG_COLOR);
            draw_seg(map,p,terminal.n,terminal.e,p->y[terminal.segment],p->x[terminal.segment],BG_COLOR);
            draw_SEGMENT(map,p,terminal.arc,terminal.segment,IT_COLOR);
           }
          else
           {
            dig_P_read_line(map,(abs(initial.arc)),&p);
#ifdef DEBUG_N
fprintf(stderr,"N:  2:before valid_pt\n");
#endif DEBUG_N
/* Determine if orange plus is valid or not.                               */
            if (valid_pt(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)])==1)
             {
              draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],IT_COLOR);
             }
            else
             {
              dig_P_read_line(map,(abs(initial.arc)),&p);
#ifdef DEBUG_N
fprintf(stderr,"N:  3:before valid_pt\n");
#endif DEBUG_N
/* Determine if green plus is valid or not.                                */
              if (valid_pt(map,p,p->y[initial.segment],p->x[initial.segment])==1)
               {
                draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],IT_COLOR);
               }
             }
           }
         }
        else
         {
          dig_P_read_line(map,(abs(terminal.arc)),&p);
/* "terminal.segment" does not equal "initial.segment".                      */
/* Erase both partial segments of terminal segment.                           */
          draw_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,BG_COLOR);
          draw_seg(map,p,terminal.n,terminal.e,p->y[terminal.segment],p->x[terminal.segment],BG_COLOR);
/* Redraw the "yellow" segment.                                              */
          draw_SEGMENT(map,p,terminal.arc,terminal.segment,IT_COLOR);
         }
       }
      else
       {
        dig_P_read_line(map,(abs(terminal.arc)),&p);
/* "arc_t_info.count is greater than 1.                                      */
/* Erase both partial segments of terminal segment.                           */
        draw_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,BG_COLOR);
        draw_seg(map,p,terminal.n,terminal.e,p->y[terminal.segment],p->x[terminal.segment],BG_COLOR);
/* Redraw the "yellow" segment.                                              */
        draw_SEGMENT(map,p,terminal.arc,terminal.segment,IT_COLOR);
       }
      if ((*previous_letter=='E')||(*previous_letter=='N'))
       {
#ifdef DEBUG_N
fprintf(stderr,"N:  redraw orange and green\n");
#endif DEBUG_N
         dig_P_read_line(map,(abs(terminal.arc)),&p);
/* Redraw the "orange" plus for the beginning point of segment.              */
         draw_PLUS(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],BS_COLOR);
/* Redraw the "green" plus for the ending point of segment.                  */
         draw_PLUS(map,p,p->y[terminal.segment],p->x[terminal.segment],ES_COLOR);
        }
      *previous_letter = 'N';
      *next_letter = 'Z';
      return(1);
     }
    else
     {
      if (button == 3)
       {
/* "Undo" the "red" plus and return to "E" or "P".                           */
        dig_P_read_line(map,(abs(terminal.arc)),&p);
/* Draw a "black" plus over the "red" plus on the "yellow" segment.          */
        draw_PLUS(map,p,terminal.n,terminal.e,BG_COLOR);
/* Draw "yellow" segment.                                                    */
        if (arc_t_info.count==1)
         {
/* Determine if both orange and green plusses are valid or not.              */
          if ((abs(terminal.arc)==abs(initial.arc))&&(terminal.segment==initial.segment))
           {
            dig_P_read_line(map,(abs(initial.arc)),&p);
            if ((valid_pt(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)])==1)
              &&(valid_pt(map,p,p->y[initial.segment],p->x[initial.segment])==1)       )
             {
              dig_P_read_line(map,(abs(terminal.arc)),&p);
/* Erase both partial segments of terminal segment.                           */
              draw_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,BG_COLOR);
              draw_seg(map,p,terminal.n,terminal.e,p->y[terminal.segment],p->x[terminal.segment],BG_COLOR);
              draw_SEGMENT(map,p,terminal.arc,terminal.segment,IT_COLOR);
             }
            else
             {
              dig_P_read_line(map,(abs(initial.arc)),&p);
  /* Determine if orange plus is valid or not.                               */
              if (valid_pt(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)])==1)
               {
                draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],IT_COLOR);
               }
              else
               {
                dig_P_read_line(map,(abs(initial.arc)),&p);
  /* Determine if green plus is valid or not.                                */
                if (valid_pt(map,p,p->y[initial.segment],p->x[initial.segment])==1)
                 {
                  draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],IT_COLOR);
                 }
               }
             }
           }
          else
           {
            dig_P_read_line(map,(abs(terminal.arc)),&p);
/* "terminal.segment" does not equal "initial.segment".                      */
/* Erase both partial segments of terminal segment.                           */
            draw_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,BG_COLOR);
            draw_seg(map,p,terminal.n,terminal.e,p->y[terminal.segment],p->x[terminal.segment],BG_COLOR);
/* Redraw the "yellow" segment.                                              */
            draw_SEGMENT(map,p,terminal.arc,terminal.segment,IT_COLOR);
           }
         }
        else
         {
          dig_P_read_line(map,(abs(terminal.arc)),&p);
/* "arc_t_info.count is greater than 1.                                      */
/* Erase both partial segments of terminal segment.                           */
          draw_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,BG_COLOR);
          draw_seg(map,p,terminal.n,terminal.e,p->y[terminal.segment],p->x[terminal.segment],BG_COLOR);
/* Redraw the "yellow" segment.                                              */
          draw_SEGMENT(map,p,terminal.arc,terminal.segment,IT_COLOR);
         }
/* Draw next "initial point" plus as "red".                                  */ 
        draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw "last measured point" plus as "violet".                              */
        draw_PLUS(map,p,last_m_pt.n,last_m_pt.e,LM_COLOR);
/* Draw next "present point" plus as "white".                                */ 
        draw_PLUS(map,p,present.n,present.e,PP_COLOR);
        if (*previous_letter=='E')
         {
          dig_P_read_line(map,(abs(terminal.arc)),&p);
/* Redraw the "orange" plus for the beginning point of segment.              */
          draw_PLUS(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],BS_COLOR);
/* Redraw the "green" plus for the ending point of segment.                  */
          draw_PLUS(map,p,p->y[terminal.segment],p->x[terminal.segment],ES_COLOR);
         }
/* "*previous_letter will be "E" or "P" and will be assigned to              */
/* "next_letter".                                                            */
        *next_letter = *previous_letter;
        *previous_letter = 'N';
        return(1);
       }
     }
   }
 }
