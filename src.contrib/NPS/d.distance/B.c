/* B:  This function confirms whether you have selected the initial segment  */
/* that you want or not.  If it is the correct segment that you want then    */
/* you will go to function "C" to select the point on that "yellow" segment. */
/* If it is not the correct segment then you will go back to function "A" to */
/* reselect the initial segment.                                             */
#include "distance.h"
int
B(next_letter,previous_letter,map,p)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  extern int draw_SEGMENT();
  extern int draw_NODE();

#ifdef DEBUG
fprintf(stderr,"B\n");
#endif DEBUG
  sprintf(line1,"Left Button:   Go to next menu to select initial point on \"yellow\" segment.    ");
  sprintf(line2,"Middle Button: Abort.                                                          ");
  sprintf(line3,"Right Button:  Undo previous selection of \"yellow\" segment; go back one menu.  ");
  strcpy(line4,"                                                                               ");
  strcpy(line5,"                                                                               ");
  strcpy(line6,"                                                                               ");
  strcpy(line7,"                                                                               ");
  strcpy(line8,"                                                                               ");
  term(line1,line2,line3,line4,line5,line6,line7,line8);
  R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
  if (button == 1)
   {
    *previous_letter = 'B';
    *next_letter = 'C';
    return(1);
   }
  else
   {
    if (button == 2)
     {
      *previous_letter = 'B';
      *next_letter = 'Z';
      return(1);
     }
    else
     {
      if (button == 3)
       {
/* Erase "yellow" segment and redraw it as the original color.               */
/* Then return to function "A" and select segment again.                     */
        if ( (initial.segment%2) != 0)
         {
          draw_SEGMENT(map,p,initial.arc,initial.segment,IA_COLOR1);
         }
        else
         {
          draw_SEGMENT(map,p,initial.arc,initial.segment,IA_COLOR2);
         }
/* Redraw the node plusses for this arc (initial.arc).                      */
        draw_NODE(map,p,map->Line[abs(initial.arc)].N1,IN_COLOR);
        draw_NODE(map,p,map->Line[abs(initial.arc)].N2,IN_COLOR);
        *previous_letter = 'B';
        *next_letter = 'A';
        return(1);
       }
     }
   }
 }
