/* E:  If you choose a mid-point of the segment with the right button then   */
/*     this function will assign values "n", "e", "indicator", and "node"    */
/*     to structure:  "terminal".                                            */ 
/* Function "E" will allow the user to select the "terminal point" on the    */
/* "terminal segment".                                                       */
#include "distance.h"
int
E(next_letter,previous_letter,map,p)
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
  extern int pt_in_WIND();
  extern int valid_pt();
  extern int term();
  int dig_P_read_line();
  double D_d_to_u_col();
  double D_d_to_u_row();
  double north, east;
  double temp_north, temp_east;
  int type;
  double d;
  char seg_ind, seg_type;
  double dist;

#ifdef DEBUG
fprintf(stderr,"E\n");
#endif DEBUG
  type = LINE | AREA;
  dig_P_read_line(map,(abs(terminal.arc)),&p);
/* Draw orange plus on terminal segment.                                     */
  draw_PLUS(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],BS_COLOR);
/* Draw green plus on terminal segment.                                      */
  draw_PLUS(map,p,p->y[terminal.segment],p->x[terminal.segment],ES_COLOR);
  sprintf(line1,"Left Button:   Terminal point is at \"orange\" or \"green\" plus.                  ");
  sprintf(line2,"Middle Button: Abort.                                                          ");
  sprintf(line3,"Right Button:  Select terminal point (with cross-hairs) on \"yellow\" segment.   ");
/* Will only have "last_m_pt" values after "arc_t_info.count" > 1.           */
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
    R_get_location_with_pointer(&screen_x, &screen_y, &button);
    east  = D_d_to_u_col((double)screen_x);
    north = D_d_to_u_row((double)screen_y);
    if (button == 1)
     {
/* "Terminal point" is at the "orange" or "green" plus.                      */
      *previous_letter = 'E';
      *next_letter = 'M';
      return(1);
     }
    else
     {
      if (button == 2)
       {
        *previous_letter = 'E';
        *next_letter = 'Z';
        return(1);
       }
      else
       {
        if (button == 3)
         {
/* Select the "terminal point" (with cross-hairs) on the terminal segment.   */
          temp_north = north;
          temp_east  = east;
/* Function "pt_on_seg" will return the new "(north,east)" point that is on  */
/* the segment itself.                                                       */
          pt_on_seg(&seg_ind,&seg_type,p->x[(terminal.segment-1)],p->y[(terminal.segment-1)],p->x[terminal.segment],p->y[terminal.segment],temp_east,temp_north,&east,&north,&dist);
/* Make sure calculated point (east,north) is inside WIND.                   */
          if ( pt_in_WIND(north,east) )
           {
/* Must restrict which point on segment to choose if initial arc and         */
/* terminal arc are the same.                                                */
            if (arc_t_info.count==1)
             {
/* "arc_t_info.count" equals 1.                                              */
/* If "arc_t_info.count" equals 1 then initial arc and terminal arc are      */
/* the same.                                                                 */
              if ((abs(initial.arc)==abs(terminal.arc))&&(initial.segment==terminal.segment))
               {
/* Determine if point (north,east) is a "valid" point or not.                */
                if (valid_pt(map,p,north,east))
                 {
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
/* Determine if point (north,east) is the beginning point of the arc or not. */
                    if ( (north==map->Node[map->Line[abs(terminal.arc)].N1].y)&&
                         (east ==map->Node[map->Line[abs(terminal.arc)].N1].x)  )
                     {
/* "point" is a terminal point and node point N1.                            */
                      terminal.indicator = '6';
                      terminal.node = map->Line[abs(terminal.arc)].N1;
                     }
                    else 
                     {
/* "point" is a terminal point and the beginning point of segment but        */
/* NOT a node point.                                                         */
                      terminal.indicator = '8';
                      terminal.node = 0;
                     }
                   }
                  else
                   {
                    if (seg_ind=='E')
                     {
/* Point (north,east) is the ending point of the segment.                    */
/* Determine if point (north,east) is the ending point of the arc or not.    */
                      if ( (north==map->Node[map->Line[abs(terminal.arc)].N2].y)&&
                           (east ==map->Node[map->Line[abs(terminal.arc)].N2].x)  )
                       {
/* "point" is a terminal point and node point N2.                            */
                        terminal.indicator = '7';
                        terminal.node = map->Line[abs(terminal.arc)].N2;
                       }
                      else 
                       {
/* "point" is a terminal point and the ending point of segment but           */
/* NOT a node point.                                                         */
                        terminal.indicator = '9';
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
                        terminal.node = 0;
                       }
                     }
                   }
/* Draw red plus on yellow segment (which is terminal point).                */
                  draw_PLUS(map,p,terminal.n,terminal.e,IP_COLOR);
                  *previous_letter = 'E';
                  *next_letter = 'N';
                  return(1);
                 }
               }
              else
               {
/* Assign values "n" and "e" for "terminal" structure.                       */
                terminal.n = north; 
                terminal.e = east;
/* Determine values for "terminal.indicator", "terminal.segment", and        */
/* "terminal.node".                                                          */
                if ( (seg_ind=='B')||(seg_ind=='P') )
                 {
/* Segment is either the beginning point of the segment "B" or the segment   */
/* is a point "P".                                                           */
/* Determine if point (north,east) is the beginning point of the arc or not. */
                  if ( (north==map->Node[map->Line[abs(terminal.arc)].N1].y)&&
                       (east ==map->Node[map->Line[abs(terminal.arc)].N1].x)  )
                   {
/* "point" is a terminal point and node point N1.                            */
                    terminal.indicator = '6';
                    terminal.node = map->Line[abs(terminal.arc)].N1;
                   }
                  else 
                   {
/* "point" is a terminal point and the beginning point of segment but        */
/* NOT a node point.                                                         */
                    terminal.indicator = '8';
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
                      terminal.node = map->Line[abs(terminal.arc)].N2;
                     }
                    else 
                     {
/* "point" is a terminal point and the ending point of segment but           */
/* NOT a node point.                                                         */
                      terminal.indicator = '9';
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
                      terminal.node = 0;
                     }
                   }
                 }
/* Draw red plus on yellow segment (which is terminal point).                */
                draw_PLUS(map,p,terminal.n,terminal.e,IP_COLOR);
                *previous_letter = 'E';
                *next_letter = 'N';
                return(1);
               }
             }
            else
             {
/* "arc_t_info.count is greater than 1.                                      */
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
                  terminal.node = map->Line[abs(terminal.arc)].N1;
                 }
                else 
                 {
/* "point" is a terminal point and the beginning point of segment but        */
/* NOT a node point.                                                         */
                  terminal.indicator = '8';
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
                    terminal.node = map->Line[abs(terminal.arc)].N2;
                   }
                  else 
                   {
/* "point" is a terminal point and the ending point of segment but           */
/* NOT a node point.                                                         */
                    terminal.indicator = '9';
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
                    terminal.node = 0;
                   }
                 }
               }
/* Draw red plus on yellow segment (which is terminal point).                */
              draw_PLUS(map,p,terminal.n,terminal.e,IP_COLOR);
              *previous_letter = 'E';
              *next_letter = 'N';
              return(1);
             }
           }
         }
       }
     }
   }
 }
