#include "distance.h"
int
I(next_letter,previous_letter,map,p,near_node_num)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 int *near_node_num;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  extern int pt_in_WIND();
  extern int term();
  extern int nearest_node();
  extern int draw_PLUS();
  double north, east;
  double D_d_to_u_col() ;
  double D_d_to_u_row() ;

#ifdef DEBUG
fprintf(stderr,"I\n");
#endif DEBUG
  sprintf(line1,"Left Button:   Select node point (with cross-hairs).                           ");
  sprintf(line2,"Middle Button: End;  (Go to next menu to Exit, Print, Extract, or Start Over). ");
  sprintf(line3,"Right Button:  Select node point (with cross-hairs).                           ");
  if (node_t_info.count==0)
   {
    strcpy(line4,"                                                                               ");
    strcpy(line5,"You are measuring direct distances from existing node point to node point.     ");
    strcpy(line6,"You only need to use the cross-hairs** when you see \"cross-hairs\" in the menu. ");
    strcpy(line7,"The rest of the time, you only need to use the three buttons on the mouse.     ");
    strcpy(line8,"** Your graphics screen may not have \"cross-hairs\" but an \"arrow\" instead.     ");
   }
  else
   {
    if (node_t_info.count>1)
     {
      sprintf(line4,"|  Initial Point  |  Terminal Point | Distance from Initial                   |"); 
      sprintf(line5,"|  \"red\" plus     |  \"violet\" plus  | to Terminal Point                       |");
      ptr_node_var = ptr_node + (node_t_info.count-1);
      sprintf(line6,"| N %13.2f | N %13.2f | %14.3f meters (cumulative dist.)|",map->Node[ptr_node->node_number].y,map->Node[ptr_node_var->node_number].y,node_t_info.cum_dist);
      sprintf(line7,"| E %13.2f | E %13.2f | %14.3f meters (shortest dist.)  |",map->Node[ptr_node->node_number].x,map->Node[ptr_node_var->node_number].x,node_t_info.dir_dist);
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
   }
  term(line1,line2,line3,line4,line5,line6,line7,line8);
  while(1)
   {
    R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
    if (button == 1 || button == 3)
     {
      east  = D_d_to_u_col((double)screen_x) ;
      north = D_d_to_u_row((double)screen_y) ;
      if ( pt_in_WIND(north,east) )
       {
        if (nearest_node(east,north,map,near_node_num)==1)
         {
          if (pt_in_WIND(map->Node[*near_node_num].y,map->Node[*near_node_num].x))
           {
/* Draw "green" plus.                                                        */
            draw_PLUS(map,p,map->Node[*near_node_num].y,map->Node[*near_node_num].x,N1_COLOR);
            *previous_letter = 'I';
            *next_letter = 'T';
            return(1);
           }
         }
       }
     }
    else
     {
      if (button == 2)
       {
        total_points += 1;
        *previous_letter = 'I';
        *next_letter = 'U';
        return(1);
       }
     }
   }
 }
