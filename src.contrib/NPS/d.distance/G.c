#include "distance.h"
int
G(next_letter,previous_letter,map,p)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  extern int term();
  extern int erase_o_g();
  extern int draw_PLUS();
  extern int draw_SEGMENT();

#ifdef DEBUG
fprintf(stderr,"G\n");
#endif DEBUG
  sprintf(line1,"Left Button:   Initial point (\"red plus\") is correct.                          ");
  sprintf(line2,"Middle Button: Abort.                                                          ");
  sprintf(line3,"Right Button:  Initial point (\"red plus\") is NOT correct.                      ");
  strcpy(line4,"                                                                               ");
  strcpy(line5,"                                                                               ");
  strcpy(line6,"                                                                               ");
  strcpy(line7,"                                                                               ");
  strcpy(line8,"                                                                               ");
  term(line1,line2,line3,line4,line5,line6,line7,line8);
  R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
  if (button == 1)
   {
    *previous_letter = 'G';
    *next_letter = 'D';
/* Erase "orange" and "green" plusses and the adjoining segments to          */
/* those two plusses.                                                        */
    erase_o_g(map,p,'I');
    return(1);
   }
  else
   {
    if (button == 2)
     {
      *previous_letter = 'G';
      *next_letter = 'Z';
      return(1);
     }
    else
     {
      if (button == 3)
       {
        *previous_letter = 'G';
        *next_letter = 'C';
/* "Undo" the "red" plus and return to "C".                                  */
        dig_P_read_line(map,(abs(initial.arc)),&p);
/* Draw a "black" plus over the "red" plus on the "yellow" segment.          */
        draw_PLUS(map,p,initial.n,initial.e,BG_COLOR);
/* Erase both partial segments of initial segment.                           */
        draw_seg(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)],initial.n,initial.e,BG_COLOR);
        draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],BG_COLOR);
/* Redraw the "yellow" segment.                                              */
        draw_SEGMENT(map,p,initial.arc,initial.segment,IT_COLOR);
/* Determine if both orange and green plusses are in WIND area.              */
        if ( ( pt_in_WIND(p->y[(initial.segment-1)],p->x[(initial.segment-1)]) )
          &&  ( pt_in_WIND(p->y[initial.segment],p->x[initial.segment]) )          )
         {
/* Redraw the "orange" plus for the beginning point of segment.              */
          draw_PLUS(map,p,p->y[(initial.segment-1)],p->x[(initial.segment-1)],BS_COLOR);
/* Redraw the "green" plus for the ending point of segment.                  */
          draw_PLUS(map,p,p->y[initial.segment],p->x[initial.segment],ES_COLOR);
         }
        return(1);
       }
     }
   }
 }
