/* C:  If you choose a mid-point of the segment with the right button then   */
/*     this function will assign values "n", "e", "indicator", and "node"    */
/*     to structures:  "present" and "initial".                              */ 
/* Function "C" allows you to select the initial point on the previously     */
/* selected initial segment.                                                 */
#include "distance.h"
int
C(next_letter,previous_letter,map,p)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 {
  extern draw_PLUS();
  int screen_x, screen_y ;
  int button;
  double north, east;
  double temp_north, temp_east;
  int arc_num;
  int seg_num;
  int type;
  double d;
  char seg_ind, seg_type;
  double D_d_to_u_col();
  double D_d_to_u_row();
  int dig_P_read_line();
  extern int pt_on_seg();
  extern int pt_in_WIND();
  extern int draw_PLUS();
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  double dist;

#ifdef DEBUG
fprintf(stderr,"C\n");
#endif DEBUG
  type = LINE | AREA;
  dig_P_read_line(map,(abs(initial.arc)),&p);
/* Determine if both orange and green plusses are in WIND area */
  if ( ( pt_in_WIND(p->y[(initial.segment-1)],p->x[(initial.segment-1)]) )
   &&  ( pt_in_WIND(p->y[initial.segment],p->x[initial.segment]) )          )
   {
/* Only if end points of the initial segment are within the "WIND" area will */
/* the orange and green plusses be drawn.                                    */
/* Draw orange plus on segment */
    draw_PLUS(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)],BS_COLOR);
/* Draw green plus on segment */
    draw_PLUS(map,p,p->y[initial.segment],p->x[initial.segment],ES_COLOR);
    sprintf(line1,"Left Button:   Initial point is either \"orange\" or \"green\" plus.               ");
   }
  else
   {
/* Either one or both of the end points of the initial segment are outside   */
/* of the "WIND" area, thus the orange and green plusses will not be drawn.  */
    sprintf(line1,"Left Button:   Select initial point (with cross-hairs) on \"yellow\" segment.    ");
   }
  sprintf(line2,"Middle Button: Abort.                                                          ");
  sprintf(line3,"Right Button:  Select initial point (with cross-hairs) on \"yellow\" segment.    ");
  strcpy(line4,"                                                                               ");
  strcpy(line5,"                                                                               ");
  strcpy(line6,"                                                                               ");
  strcpy(line7,"                                                                               ");
  strcpy(line8,"                                                                               ");
  term(line1,line2,line3,line4,line5,line6,line7,line8);
  while(1)
   {
    R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
    east  = D_d_to_u_col((double)screen_x) ;
    north = D_d_to_u_row((double)screen_y) ;
/* If either orange or green plus is outside of WIND then make button 1,     */
/* button 3.                                                                 */
    if (button==1)
     {
      if ((pt_in_WIND(p->y[(initial.segment-1)],p->x[(initial.segment-1)])!=1)||
          (pt_in_WIND(p->y[initial.segment],p->x[initial.segment])!=1)          )
       {
        button = 3;
       }
     }
    if (button == 1)
     {
/* One of the end points of the initial segment will be the initial point.   */
/* You will go to function "L" to decide which end point of the initial      */
/* segment is the initial point.                                             */
      *previous_letter = 'C';
      *next_letter = 'L';
      return(1);
     }
    else
     {
      if (button == 2)
       {
        *previous_letter = 'C';
        *next_letter = 'Z';
        return(1);
       }
      else
       {
        if (button == 3)
         {
/* You will select the initial point on the initial segment with the         */
/* cross-hairs.                                                              */
          if ( pt_in_WIND(north,east) )
           {
/* Point (north,east) must be within the "WIND" area.                        */
            arc_num = dig_point_to_line(map,east,north,type);
            if (arc_num == (abs(initial.arc)) )
             {
              seg_num = dig_check_dist(map,arc_num,east,north,&d);
              if (seg_num == initial.segment ) 
               {
                temp_north = north;
                temp_east  = east;
/* Function "pt_on_seg" will return the new "(north,east)" point that is on  */
/* the segment itself.                                                       */
                pt_on_seg(&seg_ind,&seg_type,p->x[(initial.segment-1)],p->y[(initial.segment-1)],p->x[initial.segment],p->y[initial.segment],temp_east,temp_north,&east,&north,&dist);
/* Make sure calculated point (east,north) is inside WIND.                   */
                if ( pt_in_WIND(north,east) )
                 {
/* Point (north,east) must be within the "WIND" area.                        */
/* Determine value for "present.indicator".                                  */
                  if ( (seg_ind=='B')||(seg_ind=='P') )
                   {
/* Determine if point (north,east) is beginning point of the arc or not.     */
                    if ( (north==map->Node[map->Line[abs(arc_num)].N1].y)&&
                         (east ==map->Node[map->Line[abs(arc_num)].N1].x)  )
                     {
/* "present point" is initial point and node point N1.                       */
                      present.indicator = '1';
                      present.node = map->Line[abs(arc_num)].N1;
                     }
                    else 
                     {
/* "present point" is initial point and the beginning of segment, but        */
/* NOT a node point.                                                         */
                      present.indicator = '3';
                      present.node = 0;
                     }
                   }
                  else
                   {
                    if (seg_ind=='E')
                     {
/* Determine if point (north,east) is ending point of the arc or not.        */
                      if ( (north==map->Node[map->Line[abs(arc_num)].N2].y)&&
                           (east ==map->Node[map->Line[abs(arc_num)].N2].x)  )
                       {
/* "present point" is initial point and the node point N2.                   */
                        present.indicator = '2';
                        present.node = map->Line[abs(arc_num)].N2;
                        present.arc = -arc_num;
                       }
                      else 
                       {
/* "present point" is initial point and the ending point of the segment,     */ 
/* but NOT a node point.                                                     */
                        present.indicator = '4';
                        present.node = 0;
                       }
                     }
                    else
                     {
                      if (seg_ind=='M')
                       {
/* "present point" is initial point and the mid-point of the segment,        */
/* and NOT a node point.                                                     */
                        present.indicator = '5';
                        present.node = 0;
                       }
                     }
                   }
                 }
/* Assign values for point in "present" structure.                           */
                present.n = north; 
                present.e = east;
/* Assign values for "initial" structure.                                    */
                initial.n = north;
                initial.e = east;
                initial.indicator = present.indicator;
                initial.node = present.node;
/* Set "count" value in "arc_t_info" structure for position in "arc_table"   */
                arc_t_info.count = 0;
/* Draw red plus on yellow segment (which is initial point).                 */
                draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
                *previous_letter = 'C';
                *next_letter = 'G';
                return(1);
               }
             }
           }
         }
       }
     }
   }
 }
