/* P:  If you choose a mid-point of the segment with the right or left       */
/*     button then this function will assign values "n", "e", "indicator",   */
/*     and "node" to structure:  "terminal".                                 */ 
/* Function "P" allows the user to select the terminal point on the terminal */
/* segment.  One or both of the end points of the terminal segment are not   */
/* "valid" or one or both of the end points of the terminal segment are not  */ 
/* within the "WIND" area.                                                   */
#include "distance.h"
int
P(next_letter,previous_letter,map,p)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  extern int draw_PLUS();
  extern int pt_on_seg();
  extern int nearest_seg();
  extern int pt_in_WIND();
  extern int valid_pt();
  extern int term();
  double D_d_to_u_col();
  double D_d_to_u_row();
  int dig_P_read_line();
  double north, east;
  double temp_north, temp_east;
  int near_seg;
  int type;
  double d;
  char seg_ind, seg_type;
  double dist;

#ifdef DEBUG
fprintf(stderr,"P\n");
#endif DEBUG
#ifdef DEBUG_P
fprintf(stderr,"P:  arc_t_info.count=%d\n",arc_t_info.count);
fprintf(stderr,"P:  initial.arc=%d\n",initial.arc);
fprintf(stderr,"P:  initial.segment=%d\n",initial.segment);
fprintf(stderr,"P:  terminal.arc=%d\n",terminal.arc);
fprintf(stderr,"P:  terminal.segment=%d\n",terminal.segment);
#endif DEBUG_P
  type = LINE | AREA;
  dig_P_read_line(map,(abs(terminal.arc)),&p);
  sprintf(line1,"Right Button:  Select terminal point (with cross-hairs) on \"yellow\" segment.   ");
  sprintf(line2,"Middle Button: Abort.                                                          ");
  sprintf(line3,"Right Button:  Select terminal point (with cross-hairs) on \"yellow\" segment.   ");
/* Will only have "last_m_pt" values after "arc_t_info.count" > 1     */
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
  while(1)
   {
    R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
    east  = D_d_to_u_col((double)screen_x) ;
    north = D_d_to_u_row((double)screen_y) ;
    if (button == 2)
     {
      *previous_letter = 'P';
      *next_letter = 'Z';
      return(1);
     }
    else
     {
      if ((button == 1)||(button ==3))
       {
/* Determine if point (north,east) is within the "WIND" area or not.         */
        if ( pt_in_WIND(north,east) )
         {
#ifdef DEBUG_P
fprintf(stderr,"P:  point(north=%lf,east=%lf) is inside WIND\n",north,east);
#endif DEBUG_P
/* Point (north,east) is inside WIND area.                                   */
          if (nearest_seg(map,p,terminal.arc,east,north,&near_seg,&dist))
           {
#ifdef DEBUG_P
fprintf(stderr,"P:  after nearest_seg;  near_seg=%d dist=%lf\n",near_seg,dist);
#endif DEBUG_P
/* "near_seg" is the nearest segment to point (east,north).                  */
            if (near_seg==terminal.segment)
             {
/* "near_seg" is the terminal segment.                                       */
              temp_north = north;
              temp_east  = east;
/* Function "pt_on_seg" will return the new "(north,east)" point that is on  */
/* the segment itself.                                                       */
              pt_on_seg(&seg_ind,&seg_type,p->x[(near_seg-1)],p->y[(near_seg-1)],p->x[near_seg],p->y[near_seg],temp_east,temp_north,&east,&north,&dist);
#ifdef DEBUG_P
fprintf(stderr,"P:  after pt_on_seg;  temp_east=%lf temp_north=%lf north=%lf east=%lf\n",
        temp_east,temp_north,north,east);
#endif DEBUG_P
/* Determine if point (north,east) is within the "WIND" area or not.         */
              if ( pt_in_WIND(north,east) )
               {
#ifdef DEBUG_P
fprintf(stderr,"P:  point(north=%lf,east=%lf) is inside WIND\n",north,east);
#endif DEBUG_P
/* Must restrict which point on segment to choose if initial arc and         */
/* terminal arc are the same.                                                */
/* "near_seg is a valid segment */
                if ((arc_t_info.count==1)&&
                    (abs(initial.arc)==abs(terminal.arc)) )
                 {
#ifdef DEBUG_P
fprintf(stderr,"P:  arc_t_info.count EQUALS ONE\n");
fprintf(stderr,"P:  arc_t_info.count=%d, initial.arc=%d, terminal.arc=%d\n",
        arc_t_info.count,initial.arc,terminal.arc);
#endif DEBUG_P
/* "arc_t_info.count" equals 1.                                              */
/* If "arc_t_info.count" equals 1 then initial arc and terminal arc are      */
/* the same.                                                                 */
                  if (near_seg==initial.segment)
                   {
#ifdef DEBUG_P
fprintf(stderr,"P:  before calling \"valid_pt\", near_seg=%d\n",near_seg);
fprintf(stderr,"P:  before calling \"valid_pt\", north=%lf,east=%lf\n",north,east);
#endif DEBUG_P
/* "near_seg is equal to initial.segment.                                    */
                    if (valid_pt(map,p,north,east))
                     {
#ifdef DEBUG_P
fprintf(stderr,"P:  after valid_pt;  north=%lf east=%lf is VALID\n",
          north,east);
#endif DEBUG_P
/* Point (north,east) is valid.                                              */
/* Assign values "n" and "e" for "terminal" structure.                       */
                      terminal.n = north; 
                      terminal.e = east;
/* Determine values for "terminal.indicator", "terminal.segment", and        */
/* "terminal.node".                                                          */
                      if ( (seg_ind=='B')||(seg_ind=='P') )
                       {
/* Segment is either the beginning point of the segment "B" or the segment   */
/* is a point "P".                                                           */
                        if ( (north==map->Node[map->Line[abs(terminal.arc)].N1].y)&&
                             (east ==map->Node[map->Line[abs(terminal.arc)].N1].x)  )
                         {
/* "point" is a terminal point and node point N1.                            */
                          terminal.indicator = '6';
                          terminal.segment = near_seg;
                          terminal.node = map->Line[abs(terminal.arc)].N1;
                         }
                        else 
                         {
/* "point" is a terminal point and the beginning point of segment but        */
/* NOT a node point.                                                         */
                          terminal.indicator = '8';
                          terminal.segment = near_seg;
                          terminal.node = 0;
                         }
                       }
                      else
                       {
                        if (seg_ind=='E')
                         {
/* Segment is the ending point of the segment "E".                           */
                          if ( (north==map->Node[map->Line[abs(terminal.arc)].N2].y)&&
                               (east ==map->Node[map->Line[abs(terminal.arc)].N2].x)  )
                           {
/* "point" is a terminal point and node point N2.                            */
                            terminal.indicator = '7';
                            terminal.segment = near_seg;
                            terminal.node = map->Line[abs(terminal.arc)].N2;
                           }
                          else 
                           {
/* "point" is a terminal point and the ending point of segment but           */
/* NOT a node point.                                                         */
                            terminal.indicator = '9';
                            terminal.segment = near_seg;
                            terminal.node = 0;
                           }
                         }
                        else
                         {
                          if (seg_ind=='M')
                           {
/* "point" is a terminal point and a mid-point of the terminal segment but   */
/* is not a beginning or ending point of a segment nor is it a node point.   */
                            terminal.indicator = '0';
                            terminal.segment = near_seg;
                            terminal.node = 0;
                           }
                         }
                       }
/* Draw red plus on yellow segment (which is terminal point).                */
                      draw_PLUS(map,p,north,east,IP_COLOR);
/* Draw "last measured point" plus as "violet".                              */
                      if (arc_t_info.count>1)
                        draw_PLUS(map,p,last_m_pt.n,last_m_pt.e,LM_COLOR);
/* Draw next "present point" plus as "white".                                */ 
                      draw_PLUS(map,p,present.n,present.e,PP_COLOR);
                      *previous_letter = 'P';
                      *next_letter = 'N';
                      return(1);
                     }
#ifdef DEBUG_P
                    else
                     {
fprintf(stderr,"P:  after valid_pt;  north=%lf east=%lf is NOT VALID\n",
          north,east);
                     }
#endif DEBUG_P
                   }
                  else
                   {
/* "near_seg is NOT equal to initial.segment.                                */
/* Assign values "n" and "e" for "terminal" structure.                       */
                    terminal.n = north; 
                    terminal.e = east;
/* Determine values for "terminal.indicator", "terminal.segment", and        */
/* "terminal.node".                                                          */
                    if ( (seg_ind=='B')||(seg_ind=='P') )
                     {
/* Segment is either the beginning point of the segment "B" or the segment   */
/* is a point "P".                                                           */
                      if ( (north==map->Node[map->Line[abs(terminal.arc)].N1].y)&&
                           (east ==map->Node[map->Line[abs(terminal.arc)].N1].x)  )
                       {
/* "point" is a terminal point and node point N1.                            */
                        terminal.indicator = '6';
                        terminal.segment = near_seg;
                        terminal.node = map->Line[abs(terminal.arc)].N1;
                       }
                      else 
                       {
/* "point" is a terminal point and the beginning point of segment but        */
/* NOT a node point.                                                         */
                        terminal.indicator = '8';
                        terminal.segment = near_seg;
                        terminal.node = 0;
                       }
                     }
                    else
                     {
                      if (seg_ind=='E')
                       {
/* Segment is the ending point of the segment "E".                           */
                        if ( (north==map->Node[map->Line[abs(terminal.arc)].N2].y)&&
                             (east ==map->Node[map->Line[abs(terminal.arc)].N2].x)  )
                         {
/* "point" is a terminal point and node point N2.                            */
                          terminal.indicator = '7';
                          terminal.segment = near_seg;
                          terminal.node = map->Line[abs(terminal.arc)].N2;
                         }
                        else 
                         {
/* "point" is a terminal point and the ending point of segment but           */
/* NOT a node point.                                                         */
                          terminal.indicator = '9';
                          terminal.segment = near_seg;
                          terminal.node = 0;
                         }
                       }
                      else
                       {
                        if (seg_ind=='M')
                         {
/* "point" is a terminal point and a mid-point of the terminal segment but   */
/* is not a beginning or ending point of a segment nor is it a node point.   */
                          terminal.indicator = '0';
                          terminal.segment = near_seg;
                          terminal.node = 0;
                         }
                       }
                     }
/* Draw red plus on yellow segment (which is terminal point).                */
                    draw_PLUS(map,p,north,east,IP_COLOR);
/* Draw "last measured point" plus as "violet".                              */
                    if (arc_t_info.count>1)
                      draw_PLUS(map,p,last_m_pt.n,last_m_pt.e,LM_COLOR);
/* Draw next "present point" plus as "white".                                */
                    draw_PLUS(map,p,present.n,present.e,PP_COLOR);
                    *previous_letter = 'P';
                    *next_letter = 'N';
                    return(1);
                   }
                 }
                else
                 {
#ifdef DEBUG_P
fprintf(stderr,"P:  arc_t_info.count DOES NOT EQUAL ONE\n");
fprintf(stderr,"P:  arc_t_info.count=%d, initial.arc=%d, terminal.arc=%d\n",
        arc_t_info.count,initial.arc,terminal.arc);
fprintf(stderr,"P:  seg_ind=%c\n",seg_ind);
fprintf(stderr,"P:  north=%lf east=%lf\n",north,east);
#endif DEBUG_P
/* "arc_t_info.count is greater than 1 and/or "initial.arc" does not equal   */
/* "terminal.arc".                                                           */
                  terminal.n = north; 
                  terminal.e = east;
/* Determine values for "terminal.indicator", "terminal.segment", and        */
/* "terminal.node".                                                          */
                  if ( (seg_ind=='B')||(seg_ind=='P') )
                   {
/* Segment is either the beginning point of the segment "B" or the segment   */
/* is a point "P".                                                           */
                    if ( (north==map->Node[map->Line[abs(terminal.arc)].N1].y)&&
                         (east ==map->Node[map->Line[abs(terminal.arc)].N1].x)  )
                     {
/* "point" is a terminal point and node point N1.                            */
                      terminal.indicator = '6';
                      terminal.segment = near_seg;
                      terminal.node = map->Line[abs(terminal.arc)].N1;
                     }
                    else 
                     {
/* "point" is a terminal point and the beginning point of segment but        */
/* NOT a node point.                                                         */
                      terminal.indicator = '8';
                      terminal.segment = near_seg;
                      terminal.node = 0;
                     }
                   }
                  else
                   {
                    if (seg_ind=='E')
                     {
/* Segment is the ending point of the segment "E".                           */
                      if ( (north==map->Node[map->Line[abs(terminal.arc)].N2].y)&&
                           (east ==map->Node[map->Line[abs(terminal.arc)].N2].x)  )
                       {
/* "point" is a terminal point and node point N2.                            */
                        terminal.indicator = '7';
                        terminal.segment = near_seg;
                        terminal.node = map->Line[abs(terminal.arc)].N2;
                       }
                      else 
                       {
/* "point" is a terminal point and the ending point of segment but           */
/* NOT a node point.                                                         */
                        terminal.indicator = '9';
                        terminal.segment = near_seg;
                        terminal.node = 0;
                       }
                     }
                    else
                     {
                      if (seg_ind=='M')
                       {
/* "point" is a terminal point and a mid-point of the terminal segment but   */
/* is not a beginning or ending point of a segment nor is it a node point.   */
                        terminal.indicator = '0';
                        terminal.segment = near_seg;
                        terminal.node = 0;
                       }
                     }
                   }
/* Draw red plus on yellow segment (which is terminal point).                */
                  draw_PLUS(map,p,north,east,IP_COLOR);
/* Draw "last measured point" plus as "violet".                              */
                  if (arc_t_info.count>1)
                    draw_PLUS(map,p,last_m_pt.n,last_m_pt.e,LM_COLOR);
/* Draw next "present point" plus as "white".                                */ 
                  draw_PLUS(map,p,present.n,present.e,PP_COLOR);
                  *previous_letter = 'P';
                  *next_letter = 'N';
                  return(1);
                 }
               }
#ifdef DEBUG_P
              else
               {
fprintf(stderr,"P:  point(north=%lf,east=%lf) is NOT inside WIND\n",north,east);
               }
#endif DEBUG_P
             }
           }
         }
       }
     }
   }
 }
