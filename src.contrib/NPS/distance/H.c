/* H:  This function will allow the user to go to the next arc after the     */
/* "white" plus or to select the terminal segment on the "green/violet" arc. */
#include "distance.h"
int
H(next_letter,previous_letter,map,p)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  extern int draw_SEGMENT();
  extern int draw_PLUS();
  extern int draw_seg();
  extern int valid_seg();
  extern int valid_pt();
  extern int pt_in_WIND();
  extern int term();
  double north, east;
  double D_d_to_u_col();
  double D_d_to_u_row();
  int arc_num;
  int seg_num;
  int type;
  double d;
  static char prev_letter_D;

#ifdef DEBUG
fprintf(stderr,"H\n");
#endif DEBUG
  type = LINE | AREA;
  sprintf(line1,"Left Button:   Go to menu to select next arc after \"white plus\".               ");
  sprintf(line2,"Middle Button: Abort.                                                          ");
  sprintf(line3,"Right Button:  Select terminal segment (with cross-hairs) on \"green/violet\".   ");
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
#ifdef DEBUG_H
fprintf(stderr,"H:  BEFORE CHANGE\n");
fprintf(stderr,"H:  *previous_letter=%c\n",*previous_letter);
fprintf(stderr,"H:  *next_letter=%c\n",*next_letter);
fprintf(stderr,"H:  prev_letter_D=%c\n",prev_letter_D);
fprintf(stderr,"H: present.arc=%d\n",present.arc);
fprintf(stderr,"H: initial.arc=%d\n",initial.arc);
fprintf(stderr,"H: terminal.arc=%d\n\n",terminal.arc);
#endif DEBUG_H
  if (*previous_letter=='D')
    prev_letter_D = *previous_letter;
  else
   {
    if (*previous_letter=='O')
     {
/* Switch back the value of "present.arc".                                   */
      present.arc = -(present.arc);
/* Set "*previous_letter" to 'D'.                                            */
      *previous_letter = prev_letter_D;
     }
    else
     {
/* "*previous_letter" could be 'H' (If coming from 'Z').  Change to 'D'.     */
      *previous_letter = prev_letter_D;
     }
   }
#ifdef DEBUG_H
fprintf(stderr,"H:  AFTER CHANGE\n");
fprintf(stderr,"H:  *previous_letter=%c\n",*previous_letter);
fprintf(stderr,"H:  *next_letter=%c\n",*next_letter);
fprintf(stderr,"H:  prev_letter_D=%c\n",prev_letter_D);
fprintf(stderr,"H: present.arc=%d\n",present.arc);
fprintf(stderr,"H: initial.arc=%d\n",initial.arc);
fprintf(stderr,"H: terminal.arc=%d\n",terminal.arc);
#endif DEBUG_H
  while(1)
   {
    R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
    if (button == 1)
     {
/* Go to menu to select next arc after the "white" plus.                     */
      *previous_letter = 'H';
      *next_letter = 'D';
      return(1);
     }
    else
     {
      if (button == 2)
       {
        *previous_letter = 'H';
        *next_letter = 'Z';
        return(1);
       }
      else
       {
        if (button == 3)
         {
/* Select terminal segment (with cross-hairs) on the "green/violet" arc.     */
          east  = D_d_to_u_col((double)screen_x);
          north = D_d_to_u_row((double)screen_y);
/* Determine if point (north,east) is within the "WIND" area.                */
          if ( pt_in_WIND(north,east) )
           {
            arc_num = dig_point_to_line(map,east,north,type);
            if (arc_num == abs(present.arc) )
             {
              seg_num = dig_check_dist(map,arc_num,east,north,&d);
              if (seg_num > 0)
               {
/* Switch direction of "present.arc" to go the opposite way and              */
/* then assign "opposite direction value" to "terminal.arc" value.           */
                  if (*previous_letter=='D')
                   {
                    present.arc = -(present.arc);
                    terminal.arc = present.arc;
                   }
/* Must restrict which segment to choose if initial arc and terminal arc     */
/* are the same.                                                             */
                if (arc_t_info.count==1)
                 {
/* "arc_t_info.count" equals 1.                                              */
/* If "arc_t_info.count" equals 1 then initial arc and terminal arc are      */
/* the same.                                                                 */

/* Determine if "seg_num" is a "valid" segment or not.                       */
                  if (valid_seg(seg_num))
                   {
/* Color chosen terminal segment "yellow".                                   */
/* Determine if both orange and green plusses are valid or not.              */
                    if ((abs(arc_num)==abs(initial.arc))&&(seg_num==initial.segment))
                     {
                      dig_P_read_line(map,(abs(initial.arc)),&p);
                      if ((valid_pt(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)])==1)
                        &&(valid_pt(map,p,p->y[initial.segment],p->x[initial.segment])==1)       )
                       {
                        draw_SEGMENT(map,p,arc_num,seg_num,IT_COLOR);
                       }
                      else
                       {
/* Determine if orange plus is valid or not.                                 */
                        if (valid_pt(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)])==1)
                         {
                          draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],IT_COLOR);
                         }
                        else
                         {
/* Determine if green plus is valid or not.                                  */
                          if (valid_pt(map,p,p->y[initial.segment],p->x[initial.segment])==1)
                           {
                            draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],IT_COLOR);
                           }
                          else
                           {
#ifdef DEBUG_H
fprintf(stderr,"H: neither orange or green are valid\n");
fprintf(stderr,"H: initial.segment=%d seg_num=%d\n",initial.segment,seg_num);
#endif DEBUG_H
                           }
                         }
                       }
                     }
                    else
                     {
/* Draw "yellow" segment.                                                    */
                      draw_SEGMENT(map,p,arc_num,seg_num,IT_COLOR);
                     }
/* Assign "present.arc" value and "seg_num" to values in the                 */
/* "terminal" structure.                                                     */
/* Switch direction of "present.arc" to go the opposite way and              */
/* then assign "opposite direction value" to "terminal.arc" value.           */
/*
                    if (*previous_letter=='D')
                      terminal.arc = -present.arc;
*/
                    terminal.segment = seg_num;
/* Draw "initial point" as "red" plus */ 
                    draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw "last measured point" plus as "violet" */
                    if (arc_t_info.count>1)
                      draw_PLUS(map,p,last_m_pt.n,last_m_pt.e,LM_COLOR);
/* Draw next "present point" plus as "white" */ 
                    draw_PLUS(map,p,present.n,present.e,PP_COLOR);
                    *previous_letter = 'H';
                    *next_letter = 'O';
                    return(1);
                   }
                 }
                else
                 {
/* "arc_t_info.count" is greater than 1.                                     */
/* Erase chosen terminal segment before drawing in "yellow".                 */
                  draw_SEGMENT(map,p,arc_num,seg_num,BG_COLOR);
/* Color chosen terminal segment "yellow".                                   */
                  draw_SEGMENT(map,p,arc_num,seg_num,IT_COLOR);
/* Assign "present.arc" value and "seg_num" to values in the                 */
/* "terminal" structure.                                                     */
/* Switch direction of "present.arc" to go the opposite way and              */
/* then assign "opposite direction value" to "terminal.arc" value.           */
/*
                  if (*previous_letter=='D')
                    terminal.arc = -present.arc;
*/
                  terminal.segment = seg_num;
/* Draw "initial point" as "red" plus.                                       */ 
                  draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw "last measured point" plus as "violet".                              */
                  if (arc_t_info.count>1)
                    draw_PLUS(map,p,last_m_pt.n,last_m_pt.e,LM_COLOR);
/* Draw next "present point" plus as "white".                                */ 
                  draw_PLUS(map,p,present.n,present.e,PP_COLOR);
                  *previous_letter = 'H';
                  *next_letter = 'O';
                  return(1);
                 }
               }
             }
           }
         }
       }
     }
   }
 }
