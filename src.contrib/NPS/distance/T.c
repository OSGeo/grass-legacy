#include "distance.h"
int
T(next_letter,previous_letter,map,p,near_node_num)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 int *near_node_num;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  char DPE_flag;
  int first_node_num, last_node_num;
  extern int pt_in_WIND();
  extern int term();
  extern int draw_PLUS();

#ifdef DEBUG
fprintf(stderr,"T\n");
#endif DEBUG
  sprintf(line1,"Left Button:   Node point (\"green\" plus) is correct.                           ");
  sprintf(line2,"Middle Button: End;  (Go to next menu to Exit, Print, Extract, or Start Over). ");
  sprintf(line3,"Right Button:  Node point (\"green\" plus) is NOT correct.                       ");
  if (node_t_info.count <= 1)
   {
    strcpy(line4,"                                                                               ");
    strcpy(line5,"                                                                               ");
    strcpy(line6,"                                                                               ");
    strcpy(line7,"                                                                               ");
    strcpy(line8,"                                                                               ");
   }
  else
   {
    sprintf(line4,"|  Initial Point  |  Terminal Point | Distance from Initial                   |"); 
    sprintf(line5,"|  \"red\" plus     |  \"violet\" plus  | to Terminal Point                       |");
    ptr_node_var = ptr_node + (node_t_info.count-1);
    sprintf(line6,"| N %13.2f | N %13.2f | %14.3f meters (cumulative dist.)|",map->Node[ptr_node->node_number].y,map->Node[ptr_node_var->node_number].y,node_t_info.cum_dist);
    sprintf(line7,"| E %13.2f | E %13.2f | %14.3f meters (shortest dist.)  |",map->Node[ptr_node->node_number].x,map->Node[ptr_node_var->node_number].x,node_t_info.dir_dist);
    strcpy(line8,"                                                                               ");
   }
  term(line1,line2,line3,line4,line5,line6,line7,line8);
/* Draw "green" plus.                                                        */
  draw_PLUS(map,p,map->Node[*near_node_num].y,map->Node[*near_node_num].x,N1_COLOR);
  while(1)
   {
    R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
    if (button == 1) 
     {
/* Assign node number to node_table.                                         */
      ptr_node_var = ptr_node + node_t_info.count;
      ptr_node_var->node_number = *near_node_num; 
/* Increment "node_t_info.count" value by 1.                                 */
      node_t_info.count += 1;
/* If value for "node_t_info.count" is equal to or greater than the value    */
/* for "node_t_info.total" then increase the memory capacity of the          */
/* structure "NODE_TABLE" by 10.                                             */
      if (node_t_info.count>=node_t_info.total)
       {
        node_t_info.total += 10;
        ptr_node=(struct NODE_TABLE *) realloc (ptr_node,(sizeof(struct NODE_TABLE)*node_t_info.total));
        if (ptr_node == NULL)
         {
          sprintf(line1,"OUT OF MEMORY using realloc for \"ptr_node\" pointer.                            ");
          strcpy(line2,"                                                                               ");
          strcpy(line3,"                                                                               ");
          strcpy(line4,"                                                                               ");
          strcpy(line5,"                                                                               ");
          strcpy(line6,"                                                                               ");
          strcpy(line7,"                                                                               ");
          strcpy(line8,"                                                                               ");
          term(line1,line2,line3,line4,line5,line6,line7,line8);
          return(0);
         }
       }
/* Draw node and lines and measure distance here.                            */
      if (node_t_info.count>1)
       {
/* Erase previous-to-last-node.                                              */
        erase_node(map,p,(ptr_node+((node_t_info.count-2)))->node_number);
        total_points = 0;
        DPE_flag = 'D';
        N_dr_me_pr_e(map,p,DPE_flag);
        first_node_num = ptr_node->node_number;
        ptr_node_var = ptr_node + (node_t_info.count-1);
        last_node_num = ptr_node_var->node_number;
/* Draw plus at first node point.                                            */
        draw_PLUS(map,p,map->Node[first_node_num].y,map->Node[first_node_num].x,N2_COLOR);
/* Draw plus at last node point.                                             */
        draw_PLUS(map,p,map->Node[last_node_num].y,map->Node[last_node_num].x,N3_COLOR);
       }
      else
       {
        draw_PLUS(map,p,map->Node[*near_node_num].y,map->Node[*near_node_num].x,N2_COLOR);
       }
      *previous_letter = 'T';
      *next_letter = 'I';
      return(1);
     }
    else
     {
      if (button == 2)
       {
/* Erase "green" plus.                                                       */
        erase_node(map,p,*near_node_num);
        if (node_t_info.count>0)
/* Draw plus at first node point.                                            */
          draw_PLUS(map,p,map->Node[ptr_node->node_number].y,map->Node[ptr_node->node_number].x,N2_COLOR);
        if (node_t_info.count>1)
/* Draw plus at last node point.                                             */
          draw_PLUS(map,p,map->Node[(ptr_node+(node_t_info.count-1))->node_number].y,map->Node[(ptr_node+(node_t_info.count-1))->node_number].x,N3_COLOR);
        total_points += 1;
        *previous_letter = 'T';
        *next_letter = 'U';
        return(1);
       }
      else
       {
        if (button == 3)
         {
/* Erase "green" plus.                                                       */
          erase_node(map,p,*near_node_num);
          if (node_t_info.count>0)
/* Draw plus at first node point.                                            */
            draw_PLUS(map,p,map->Node[ptr_node->node_number].y,map->Node[ptr_node->node_number].x,N2_COLOR);
          if (node_t_info.count>1)
/* Draw plus at last node point.                                             */
            draw_PLUS(map,p,map->Node[(ptr_node+(node_t_info.count-1))->node_number].y,map->Node[(ptr_node+(node_t_info.count-1))->node_number].x,N3_COLOR);
          *previous_letter = 'T';
          *next_letter = 'I';
          return(1);
         }
       }
     }
   }
 }
