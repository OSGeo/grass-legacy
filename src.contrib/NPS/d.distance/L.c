/* L:  This function will assign the values "n", "e", "indicator",           */
/*     and "node" to the structures: "present" and "initial".                */
/* Function "L" will allow the user to select either one of the end points   */
/* of the initial segment.                                                   */
#include "distance.h"
int
L(next_letter,previous_letter,map,p)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  extern int draw_SEGMENT();
  extern int term();
  extern int erase_o_g();

#ifdef DEBUG
fprintf(stderr,"L\n");
#endif DEBUG
  sprintf(line1,"Left Button:   Initial point is \"orange\" plus.                                 ");
  sprintf(line2,"Middle Button: Abort.                                                          ");
  sprintf(line3,"Right Button:  Initial point is \"green\" plus.                                  ");
  sprintf(line4,"|  Initial Point  |  Node Point     | Distance from Initial                   |"); 
  strcpy(line4,"                                                                               ");
  strcpy(line5,"                                                                               ");
  strcpy(line6,"                                                                               ");
  strcpy(line7,"                                                                               ");
  strcpy(line8,"                                                                               ");
  term(line1,line2,line3,line4,line5,line6,line7,line8);
  dig_P_read_line(map,(abs(present.arc)),&p);
  R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
  if (button == 1)
   {
/* Initial point is at beginning point of segment.                           */
    *previous_letter = 'L';
    *next_letter = 'D';
/* Set values for point in "present" structure.                              */
    present.n = p->y[(present.segment-1)];
    present.e = p->x[(present.segment-1)];
/* Set values for point in "initial" structure.                              */
    initial.n = p->y[(initial.segment-1)];
    initial.e = p->x[(initial.segment-1)];
/* Erase the initial segment.                                                */
    draw_SEGMENT(map,p,initial.arc,initial.segment,BG_COLOR);
/* Determine if "initial point" is node N1 or not.                           */
    if ( (initial.n == map->Node[map->Line[(abs(initial.arc))].N1].y) && 
         (initial.e == map->Node[map->Line[(abs(initial.arc))].N1].x)   )
     {
/* "point" is beginning node point of line and initial point.                */
/* Assign values to "present" structure.                                     */
      present.indicator = '1';
      present.node = map->Line[(abs(present.arc))].N1;
/* present.arc value must be positive since you are at node N1.              */
      present.arc = abs(present.arc);
/* Assign values to "initial" structure.                                     */
      initial.indicator = '1';
      initial.node = map->Line[(abs(present.arc))].N1;
/* initial.arc value must be positive since you are at node N1.              */
      initial.arc = abs(initial.arc);
     }
    else
     {
/* present.n and present.e is NOT a node point but the beginning point of    */
/* segment.                                                                  */
/* Assign values to "present" structure.                                     */
      present.indicator = '3';
      present.node = 0;
/* Assign values to "initial" structure.                                     */
      initial.indicator = '3';
      initial.node = 0;
     }
/* Erase "orange" and "green" plusses and the adjoining segments to          */
/* those two plusses.                                                        */
    erase_o_g(map,p,'I');
    return(1);
   }
  else
   {
    if (button == 2)
     {
      *previous_letter = 'L';
      *next_letter = 'Z';
      return(1);
     }
    else
     {
      if (button == 3)
       {
/* Initial point is at ending point of segment.                              */
        *previous_letter = 'L';
        *next_letter = 'D';
/* Set values for "present" structure.                                       */
        present.n = p->y[present.segment];
        present.e = p->x[present.segment];
/* Set values for "initial" structure.                                       */
        initial.n = p->y[initial.segment];
        initial.e = p->x[initial.segment];
/* Erase initial segment.                                                    */
        draw_SEGMENT(map,p,initial.arc,initial.segment,BG_COLOR);
/* Determine if "initial point" is node N2 or not.                           */
        if ( (initial.n == map->Node[map->Line[(abs(initial.arc))].N2].y) && 
             (initial.e == map->Node[map->Line[(abs(initial.arc))].N2].x)   )
         {
/* "present.n" and "present.e" is a node point and initial point             */
/* Assign values to "present" structure.                                     */
          present.indicator = '2';
          present.node = map->Line[(abs(present.arc))].N2;
/* "present.arc" value must be negative since you are at node N2.            */
          present.arc = (-(abs(present.arc)));
/* Assign values to "initial" structure.                                     */
          initial.indicator = '2';
          initial.node = map->Line[(abs(initial.arc))].N2;
/* "initial.arc" value must be negative since you are at node N2.            */
          initial.arc = (-(abs(initial.arc)));
         }
        else
         {
/* "present.n" and "present.e" is NOT a node point but the ending point of   */
/* segment.                                                                  */
/* Assign values to "present" structure.                                     */
          present.indicator = '4';
          present.node = 0;
/* Assign values to "initial" structure.                                     */
          initial.indicator = '4';
          initial.node = 0;
         }
/* Erase "orange" and "green" plusses and the adjoining segments to          */
/* those two plusses.                                                        */
        erase_o_g(map,p,'I');
        return(1);
       }
     }
   }
 }
