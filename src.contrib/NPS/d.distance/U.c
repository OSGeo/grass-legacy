#include "distance.h"
int
U(next_letter,previous_letter,map,p,overlay_flag,near_node_num)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 int overlay_flag;
 int *near_node_num;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  int first_node_num, last_node_num;
  extern int term();
  extern int draw_seg();
  extern int erase_node();

#ifdef DEBUG
fprintf(stderr,"U\n");
#endif DEBUG
  if (node_t_info.count <= 1)
   {
    sprintf(line1,"Left Button:   Did not want to \"End\".                                          ");
    sprintf(line2,"Middle Button: Exit.                                                           ");
    sprintf(line3,"Right Button:  Start all over.                                                 ");
    strcpy(line4,"                                                                               ");
    strcpy(line5,"                                                                               ");
    strcpy(line6,"                                                                               ");
    strcpy(line7,"                                                                               ");
    strcpy(line8,"                                                                               ");
   }
  else
   {
    sprintf(line1,"Left Button:   Did not want to \"End\".                                          ");
    sprintf(line2,"Middle Button: Go to menu to Exit or Print or Extract.                         ");
    sprintf(line3,"Right Button:  Start all over.                                                 ");
    sprintf(line4,"|  Initial Point  |  Terminal Point | Distance from Initial                   |"); 
    sprintf(line5,"|  \"red\" plus     |  \"violet\" plus  | to Terminal Point                       |");
    ptr_node_var = ptr_node + (node_t_info.count-1);
    sprintf(line6,"| N %13.2f | N %13.2f | %14.3f meters (cumulative dist.)|",map->Node[ptr_node->node_number].y,map->Node[ptr_node_var->node_number].y,node_t_info.cum_dist);
    sprintf(line7,"| E %13.2f | E %13.2f | %14.3f meters (shortest dist.)  |",map->Node[ptr_node->node_number].x,map->Node[ptr_node_var->node_number].x,node_t_info.dir_dist);
    strcpy(line8,"                                                                               ");
   }
  term(line1,line2,line3,line4,line5,line6,line7,line8);
  R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
  if (button == 1)
   {
    total_points -= 1;
    *next_letter = *previous_letter;
    return(1);
   }
  else
   {
    if (button == 2)
     {
      if (node_t_info.count>1)
       {
        first_node_num = ptr_node->node_number;
        ptr_node_var = ptr_node + (node_t_info.count-1);
        last_node_num = ptr_node_var->node_number;
/* Only draw "yellow" direct line if there are more than 2 points and the    */
/* first and last node number are not the same.                              */
        if ((node_t_info.count>2)&&
            (first_node_num!=last_node_num) )
         {
/* Draw "yellow" direct line distance from starting node to ending node.     */
          draw_seg(map,p,map->Node[first_node_num].y,map->Node[first_node_num].x,map->Node[last_node_num].y,map->Node[last_node_num].x,DD_COLOR);
/* Draw plus at first node point.                                            */
          draw_PLUS(map,p,map->Node[first_node_num].y,map->Node[first_node_num].x,N2_COLOR);
/* Draw plus at last node point.                                             */
          draw_PLUS(map,p,map->Node[last_node_num].y,map->Node[last_node_num].x,N3_COLOR);
         }
        *next_letter = 'V';
       }
      else
       {
        *next_letter = 'z';
       }
      return(1);
     }
    else
     {
      if (button == 3)
       {
        if (overlay_flag)
         {
          if (node_t_info.count>1)
           {
            first_node_num = ptr_node->node_number;
            ptr_node_var = ptr_node + (node_t_info.count-1);
            last_node_num = ptr_node_var->node_number;
/* Erase plus at first node point.                                           */
            draw_PLUS(map,p,map->Node[first_node_num].y,map->Node[first_node_num].x,BG_COLOR);
/* Erase plus at last node point.                                            */
            draw_PLUS(map,p,map->Node[last_node_num].y,map->Node[last_node_num].x,BG_COLOR);
/* Erase "yellow" direct line distance from starting node to ending node.    */
            draw_seg(map,p,map->Node[first_node_num].y,map->Node[first_node_num].x,map->Node[last_node_num].y,map->Node[last_node_num].x,BG_COLOR);
/* Erase lines going from node to node.                                      */
            for (ptr_node_var=ptr_node+1; ptr_node_var < (ptr_node+node_t_info.count); ptr_node_var++)
             {
              draw_seg(map,p,map->Node[(ptr_node_var-1)->node_number].y,map->Node[(ptr_node_var-1)->node_number].x,map->Node[ptr_node_var->node_number].y,map->Node[ptr_node_var->node_number].x,BG_COLOR);
             }
/* Erase nodes.                                                              */
            for (ptr_node_var=ptr_node; ptr_node_var < (ptr_node+node_t_info.count); ptr_node_var++)
             {
              erase_node(map,p,ptr_node_var->node_number);
             }
           }
         }
        END(map);
        *next_letter = 'a';
        return(1);
       }
     }
   }
 }
