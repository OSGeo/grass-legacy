/* A:  This function will assign values "arc" and "segment" to the           */
/*     structures:  "present" and "initial".                                 */
#include "distance.h"
int
A(next_letter,map,p)
 char *next_letter;
 struct Map_info *map;
 struct line_pnts *p;
 {
  int screen_x, screen_y ;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  int button;
  extern draw_SEGMENT();
  extern int pt_in_WIND();
  double north, east;
  double D_d_to_u_col() ;
  double D_d_to_u_row() ;
  int arc_num;
  int seg_num;
  int type;
  double d;

#ifdef DEBUG
fprintf(stderr,"A\n");
#endif DEBUG
  type = LINE | AREA;
  sprintf(line1,"Left Button:   Select segment (with cross-hairs) containing the initial point. ");
  sprintf(line2,"Middle Button: Exit \"d.distance\" program.                                      ");
  sprintf(line3,"Right Button:  Select segment (with cross-hairs) containing the initial point. ");
/* Provide user with information.                                            */
  strcpy(line4,"                                                                               ");
  strcpy(line5,"You are measuring from and to any point on existing arcs.                      ");
  strcpy(line6,"You only need to use the cross-hairs** when you see \"cross-hairs\" in the menu. ");
  strcpy(line7,"The rest of the time, you only need to use the three buttons on the mouse.     ");
  strcpy(line8,"** Your graphics screen may not have \"cross-hairs\" but an \"arrow\" instead.     ");
  term(line1,line2,line3,line4,line5,line6,line7,line8);
  while (1) 
   {
    R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
    if (button == 2)
     {
      *next_letter = 'z';
      return(1);
     }
    else
     {
      if ((button == 1) || (button == 3) )
       {
        east  = D_d_to_u_col((double)screen_x) ;
        north = D_d_to_u_row((double)screen_y) ;
        if ( pt_in_WIND(north,east) )
         {
          arc_num = dig_point_to_line(map,east,north,type);
          if (arc_num > 0)
           {
            seg_num = dig_check_dist(map,arc_num,east,north,&d);
            if (seg_num > 0)
             {
/* Erase chosen initial segment before drawing it in "yellow".               */
              draw_SEGMENT(map,p,arc_num,seg_num,BG_COLOR);
/* Color chosen segment "yellow".                                            */
              draw_SEGMENT(map,p,arc_num,seg_num,IT_COLOR);
/* Transfer present values for point to "present" structure.                 */
              present.arc = arc_num;
              present.segment = seg_num;
/* Assign values for arc_num and seg_num  to "initial" structure.            */
              initial.arc = arc_num;
              initial.segment = seg_num;
              *next_letter = 'B';
              return(1);
             }
           }
         }
       }
     }
   }
 }
