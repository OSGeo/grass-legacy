/* O:  This function confirms whether the user has selected the correct      */
/* terminal segment or not.                                                  */
#include "distance.h"
int
O(next_letter,previous_letter,map,p)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  char color[19];
  extern int term();
  extern int pt_in_WIND();
  extern int draw_SEGMENT();
  extern int draw_seg();
  extern int draw_NODE();
  extern int draw_PLUS();
  extern int valid_pt();
  static char prev_letter_H_J;

#ifdef DEBUG
fprintf(stderr,"O\n");
#endif DEBUG
  sprintf(line1,"Left Button:   Highlighted \"yellow\" segment is correct terminal segment.       ");
  sprintf(line2,"Middle Button: Abort.                                                          ");
  sprintf(line3,"Right Button:  Highlighted \"yellow\" segment is NOT the terminal segment.       ");
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
  if ((*previous_letter=='H')||(*previous_letter=='J'))
    prev_letter_H_J = *previous_letter;
  else
    *previous_letter = prev_letter_H_J;
  term(line1,line2,line3,line4,line5,line6,line7,line8);
#ifdef DEBUG_O
fprintf(stderr,"O:  *previous_letter=%c\n",*previous_letter);
fprintf(stderr,"O:  *next_letter=%c\n",*next_letter);
fprintf(stderr,"O:  prev_letter_H_J=%c\n",prev_letter_H_J);
fprintf(stderr,"O:  present.arc=%d\n",present.arc);
fprintf(stderr,"O:  present.segment=%d\n",present.segment);
fprintf(stderr,"O:  initial.arc=%d\n",initial.arc);
fprintf(stderr,"O:  terminal.arc=%d\n",terminal.arc);
fprintf(stderr,"O:  terminal.segment=%d\n",terminal.segment);
#endif DEBUG_O
  R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
#ifdef DEBUG_O
fprintf(stderr,"O:  button=%d\n",button);
#endif DEBUG_O
  if (button == 1)
   {
#ifdef DEBUG_O
fprintf(stderr,"O:  yellow segment is correct segment.\n");
#endif DEBUG_O
    dig_P_read_line(map,(abs(terminal.arc)),&p);
    *previous_letter = 'O';
/* Determine if both beginning and ending point of terminal segment          */
/* is inside the window or not.                                              */
    if ((pt_in_WIND(p->y[terminal.segment-1],p->x[terminal.segment-1])==1)&&
        (pt_in_WIND(p->y[terminal.segment],p->x[terminal.segment])==1)      )
     {
#ifdef DEBUG_O
fprintf(stderr,"O:  both end points of yellow segment are in WIND.\n");
#endif DEBUG_O
      if ((arc_t_info.count==1)&&(initial.arc==terminal.arc)&&(initial.segment==terminal.segment))
       {
#ifdef DEBUG_O
fprintf(stderr,"O:  initial.segment and terminal.segment are the same.\n");
#endif DEBUG_O
        dig_P_read_line(map,(abs(initial.arc)),&p);
/* "initial.arc" equals "terminal.arc" and "initial.segment" equals          */
/* "terminal.segment".                                                       */
        if ((valid_pt(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)])==1)
          &&(valid_pt(map,p,p->y[initial.segment],p->x[initial.segment])==1)       )
         {
#ifdef DEBUG_O
fprintf(stderr,"O:  both end points of yellow segment are valid.\n");
#endif DEBUG_O
/* Both end points of the terminal segment are "valid".                      */
          *next_letter = 'E';
         }
        else
         {
#ifdef DEBUG_O
fprintf(stderr,"O:  both end points of yellow segment are NOT valid.\n");
#endif DEBUG_O
/* One or both of the end points of the terminal segment are not "valid".    */
          *next_letter = 'P';
         }
       }
      else
       {
/* The "terminal.arc" and the "initial.arc are not equal or                  */
/* "arc_t_info.count" is not equal to 1.                                     */
        *next_letter = 'E';
       }
     }
    else
     {
#ifdef DEBUG_O
fprintf(stderr,"O:  both end points of yellow segment are NOT in WIND.\n");
#endif DEBUG_O
/* One or both of the end points of the terminal segment is not "valid".     */
      *next_letter = 'P';
     }
    return(1);
   }
  else
   {
    if (button == 2)
     {
      *previous_letter = 'O';
      *next_letter = 'Z';
      return(1);
     }
    else
     {
      if (button == 3)
       {
#ifdef DEBUG_O
fprintf(stderr,"O:  redraw terminal segment: %d for terminal arc: %d\n",
        terminal.segment,terminal.arc);
fprintf(stderr,"O:  button=%d\n",button);
fprintf(stderr,"O:  arc_t_info.count=%d\n",arc_t_info.count);
fprintf(stderr,"O:  initial segment: %d;  initial arc: %d\n",
        initial.segment,initial.arc);
#endif DEBUG_O
/* Redraw previous "yellow" segment as "green/violet" highlighted segment.   */
        if (arc_t_info.count==1)
         {
/* Color chosen terminal segment "yellow".                                   */
/* Determine if both orange and green plusses are valid or not.              */
          if ((abs(terminal.arc)==abs(initial.arc))&&(terminal.segment==initial.segment))
           {
            dig_P_read_line(map,(abs(initial.arc)),&p);
            if ( initial.segment%2 != 0)
              strcpy(color,HL_COLOR1);
            else
              strcpy(color,HL_COLOR2);
/* Check orange and green plusses.                                           */
            if ((valid_pt(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)])==1)
              &&(valid_pt(map,p,p->y[initial.segment],p->x[initial.segment])==1)       )
             {
/* Both end points of terminal segment are "valid".                          */
              draw_SEGMENT(map,p,terminal.arc,terminal.segment,color);
             }
            else
             {
              dig_P_read_line(map,(abs(initial.arc)),&p);
/* Determine if orange plus is valid or not.                                 */
              if (valid_pt(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)])==1)
               {
                draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],color);
               }
              else
               {
                dig_P_read_line(map,(abs(initial.arc)),&p);
/* Determine if green plus is valid or not.                                  */
                if (valid_pt(map,p,p->y[initial.segment],p->x[initial.segment])==1)
                 {
                  draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],color);
                 }
               }
             }
           }
          else
           {
#ifdef DEBUG_O
fprintf(stderr,"O:  1:redraw terminal segment: %d for terminal arc: %d\n",
        terminal.segment,terminal.arc);
#endif DEBUG_O
/* "initial.segment" and "terminal.segment" are not the same.                */
            if ( terminal.segment%2 != 0)
             {
              draw_SEGMENT(map,p,terminal.arc,terminal.segment,HL_COLOR1);
             }
            else
             {
              draw_SEGMENT(map,p,terminal.arc,terminal.segment,HL_COLOR2);
             }
           }
         }
        else
         {
#ifdef DEBUG_O
fprintf(stderr,"O:  2:redraw terminal segment: %d for terminal arc: %d\n",
        terminal.segment,terminal.arc);
#endif DEBUG_O
/* "arc_t_info.count" is greater than 1.                                     */
          if ( terminal.segment%2 != 0)
           {
            draw_SEGMENT(map,p,terminal.arc,terminal.segment,HL_COLOR1);
           }
          else
           {
            draw_SEGMENT(map,p,terminal.arc,terminal.segment,HL_COLOR2);
           }  
         }
/* Draw plusses at the two end nodes of the terminal arc.                    */
        draw_NODE(map,p,map->Line[abs(terminal.arc)].N1,IN_COLOR);
        draw_NODE(map,p,map->Line[abs(terminal.arc)].N2,IN_COLOR);
/* Draw "initial point" plus as "red".                                       */ 
        draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw "last measured point" plus as "violet".                              */
        if (arc_t_info.count>1)
          draw_PLUS(map,p,last_m_pt.n,last_m_pt.e,LM_COLOR);
/* Draw next "present point" plus as "white".                                */ 
        draw_PLUS(map,p,present.n,present.e,PP_COLOR);
        *next_letter = *previous_letter;
        *previous_letter = 'O';
        return(1);
       }
     }
   }
 }
