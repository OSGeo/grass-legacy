/* Z:  This function allows the user to exit program, start over, or to      */
/* return to the function which called "Z" function.                         */
#include "distance.h"
int
Z(next_letter,previous_letter,map,p,overlay_flag)
 char *next_letter;
 char *previous_letter;
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
fprintf(stderr,"Z\n");
#endif DEBUG
  sprintf(line1,"Left Button:   Did not want to \"abort\".                                        ");
  sprintf(line2,"Middle Button: Exit \"d.distance\" program.                                      ");
  sprintf(line3,"Right Button:  Start all over.                                                 ");
  strcpy(line4,"                                                                               ");
  strcpy(line5,"                                                                               ");
  strcpy(line6,"                                                                               ");
  strcpy(line7,"                                                                               ");
  strcpy(line8,"                                                                               ");
  term(line1,line2,line3,line4,line5,line6,line7,line8);
  R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
  if (button == 1)
   {
    *next_letter = *previous_letter;
    return(1);
   }
  else
   {
    if (button == 2)
     {
      *next_letter = 'z';
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
