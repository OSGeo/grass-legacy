/* Y:  This function allows the user to print out the measurements or to not */
/* print out the measurements.  Or the user may start over.                  */
#include "distance.h"
int
Y(next_letter,map,p,overlay_flag)
 char *next_letter;
 struct Map_info *map;
 struct line_pnts *p;
 int overlay_flag;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  extern int term();
  extern int erase4over();
  extern int END();

#ifdef DEBUG
fprintf(stderr,"Y\n");
#endif DEBUG
  sprintf(line1,"Left Button:   Print out measurements then go to menu to Exit or Extract.      ");
  sprintf(line2,"Middle Button: Do NOT print out measurements; go to menu to Exit or Extract.   ");
  sprintf(line3,"Right Button:  Start all over.                                                 ");
  sprintf(line4,"|  Initial Point  |  Terminal Point | Distance from Initial                   |"); 
  sprintf(line5,"|  \"red\" plus     |  \"violet\" plus  | to Terminal Point                       |");
  sprintf(line6,"| N %13.2f | N %13.2f | %14.3f meters (distance by arcs)|",initial.n,terminal.n,terminal.arc_dist_i_t);
  sprintf(line7,"| E %13.2f | E %13.2f | %14.3f meters (shortest dist.)  |",initial.e,terminal.e,terminal.dir_dist_i_t);                 
  strcpy(line8,"                                                                               ");
  term(line1,line2,line3,line4,line5,line6,line7,line8);
  R_get_location_with_pointer(&screen_x, &screen_y, &button);
  if (button == 1)
   {
    *next_letter = 'X';
    return(1);
   }
  else
   {
    if (button == 2)
     {
      *next_letter = 'K';
      return(1);
     }
    else
     {
      if (button == 3)
       {
        if (overlay_flag)
          erase4over(map,p);
        END(map);
        *next_letter = 'a';
        return(1);
       }
     }
   }
 }
