/* J:  This function allows the user to select (with cross-hairs) the        */
/* terminal segment from the "green/violet" arc.                             */
#include "distance.h"
int
J(next_letter,previous_letter,map,p,near_node_num)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 int near_node_num;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  extern int draw_SEGMENT();
  extern int draw_PLUS();   
  extern int pt_in_WIND();
  extern int valid_seg();
  extern int valid_pt();
  extern int draw_seg();
  extern int term();
  double north, east;
  double D_d_to_u_col();
  double D_d_to_u_row();
  int type;
  double d;
  int near_seg;
  double dist;

#ifdef DEBUG
fprintf(stderr,"J\n");
#endif DEBUG
  sprintf(line1,"Left Button:   Select (with cross-hairs) terminal segment on \"green/violet\" arc");
  sprintf(line2,"Middle Button: Abort.                                                          ");
  sprintf(line3,"Right Button:  Select (with cross-hairs) terminal segment on \"green/violet\" arc");
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
/* Assign the opposite direction of the "present.arc" to the "terminal.arc"  */
/* only if you are entering "J" for the first time and coming from "D".      */
  if (*previous_letter=='D')
   {
    present.arc = -(present.arc);
    terminal.arc = present.arc;
   }
#ifdef DEBUG_J
fprintf(stderr,"J:  *previous_letter=%c\n",*previous_letter);
fprintf(stderr,"J:  *next_letter=%c\n",*next_letter);
#endif DEBUG_J
#ifdef DEBUG_J
fprintf(stderr,"J: initial.arc=%d\n",initial.arc);
fprintf(stderr,"J: present.arc=%d\n",present.arc);
fprintf(stderr,"J: terminal.arc=%d\n",terminal.arc);
#endif DEBUG_J
  while(1)
   {
    R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
    if ((button == 1) || (button == 3))
     {
      east  = D_d_to_u_col((double)screen_x) ;
      north = D_d_to_u_row((double)screen_y) ;
/* Determine if point (north,east) is within the "WIND" area.                */
      if ( pt_in_WIND(north,east) )
       {
/* Function "nearest_seg" will return the nearest segment number "near_seg". */ 
        if (nearest_seg(map,p,present.arc,east,north,&near_seg,&dist))
         {
#ifdef DEBUG_J
fprintf(stderr,"J: near_seg=%d\n",near_seg); 
fprintf(stderr,"J: present.arc=%d\n",present.arc);
fprintf(stderr,"J: arc_t_info.count=%d\n",arc_t_info.count);
#endif DEBUG_J
/* Must restrict which segment to choose if initial arc and terminal arc     */
/* are the same.                                                             */
          if (arc_t_info.count==1)
           {
/* "arc_t_info.count" equals 1.                                              */
/* If "arc_t_info.count" equals 1 then initial arc and terminal arc are      */
/* the same.                                                                 */
#ifdef DEBUG_J
fprintf(stderr,"J:  arc_t_info.count EQUALS ONE\n");
fprintf(stderr,"J: initial.arc=%d\n",initial.arc);
fprintf(stderr,"J: present.arc=%d\n",present.arc);
fprintf(stderr,"J: terminal.arc=%d\n",terminal.arc);
#endif DEBUG_J
/* Determine if "near_seg" is a "valid" segment or not.                      */
            if (valid_seg(near_seg))
             {
#ifdef DEBUG_J
fprintf(stderr,"J: near_seg=%d is VALID\n",near_seg); 
#endif DEBUG_J
/* Draw "yellow" segment.                                                    */
/* Determine if both orange and green plusses are valid or not.              */
              if ((abs(present.arc)==abs(initial.arc))&&(near_seg==initial.segment))
               {
#ifdef DEBUG_J
fprintf(stderr,"J: near_seg=%d is equal to initial.segment\n",near_seg);
fprintf(stderr,"J: present.arc=%d\n",present.arc);
fprintf(stderr,"J: initial.arc=%d\n",initial.arc);
fprintf(stderr,"J: terminal.arc=%d\n",terminal.arc);
#endif DEBUG_J
                dig_P_read_line(map,(abs(initial.arc)),&p);
                if ((valid_pt(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)])==1)
                  &&(valid_pt(map,p,p->y[initial.segment],p->x[initial.segment])==1)       )
                 {
#ifdef DEBUG_J
fprintf(stderr,"J: near_seg=%d orange and green is VALID\n",near_seg); 
#endif DEBUG_J
                  draw_SEGMENT(map,p,present.arc,near_seg,IT_COLOR);
                 }
                else
                 {
  /* Determine if orange plus is valid or not.                               */
                  if (valid_pt(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)])==1)
                   {
#ifdef DEBUG_J
fprintf(stderr,"J: near_seg=%d orange is VALID\n",near_seg); 
#endif DEBUG_J
                    draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],IT_COLOR);
                   }
                  else
                   {
  /* Determine if green plus is valid or not.                                */
                    if (valid_pt(map,p,p->y[initial.segment],p->x[initial.segment])==1)
                     {
#ifdef DEBUG_J
fprintf(stderr,"J: near_seg=%d green is VALID\n",near_seg); 
#endif DEBUG_J
                      draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],IT_COLOR);
                     }
                    else
                     {
#ifdef DEBUG_J
fprintf(stderr,"J: neither orange or green are valid\n");
#endif DEBUG_J
/*
                      if (intial.arc>0)
                       {
                        draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],IT_COLOR);
                       }
                      else
                       {
                        draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],IT_COLOR);
                       }
*/
                     }
                   }
                 }
               }
              else
               {
/* Color chosen segment "yellow".                                            */
                draw_SEGMENT(map,p,present.arc,near_seg,IT_COLOR);
               }
/* Transfer values for segment to "present" structure.                       */
              present.segment = near_seg;
/* Transfer values for segment to "terminal" structure.                      */
              terminal.segment = near_seg;
/* Draw "initial point" as "red" plus.                                       */ 
              draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw "last measured point" plus as "violet".                              */
              if (arc_t_info.count>1)
                draw_PLUS(map,p,last_m_pt.n,last_m_pt.e,LM_COLOR);
/* Draw next "present point" plus as "white".                                */ 
              draw_PLUS(map,p,present.n,present.e,PP_COLOR);
              *previous_letter = 'J';
              *next_letter = 'O';
              return(1);
             }
           }
          else
           {
#ifdef DEBUG_J
fprintf(stderr,"J: arc_t_info.count=%d and is GREATER THAN ONE\n",arc_t_info.count);
#endif DEBUG_J
/* "arc_t_info.count" is greater than 1.                                     */
/* Color chosen segment "yellow".                                            */
            draw_SEGMENT(map,p,present.arc,near_seg,IT_COLOR);
/* Transfer values for segment to "present" structure.                       */
            present.segment = near_seg;
/* Transfer values for segment to "terminal" structure.                      */
            terminal.segment = near_seg;
/* Draw "initial point" plus as "red".                                       */ 
            draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw "last measured point" plus as "violet".                              */
            if (arc_t_info.count>1)
              draw_PLUS(map,p,last_m_pt.n,last_m_pt.e,LM_COLOR);
/* Draw next "present point" plus as "white".                                */ 
            draw_PLUS(map,p,present.n,present.e,PP_COLOR);
            *previous_letter = 'J';
            *next_letter = 'O';
            return(1);
           }
         }
       }
     }
    else
     {
      if (button == 2)
       {
        *previous_letter = 'J';
        *next_letter = 'Z';
        return(1);
       }
     }
   }
 }
