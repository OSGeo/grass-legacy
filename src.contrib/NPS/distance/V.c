#include "distance.h"
int
V(next_letter,previous_letter,map,p,overlay_flag,name)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 int overlay_flag;
 char *name;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  int DPE_flag;
  int first_node_num, last_node_num;
  extern int term(); 
  extern int draw_PLUS();
  extern int draw_seg();
  extern int erase_node(); 

#ifdef DEBUG
fprintf(stderr,"V\n");
#endif DEBUG
  if (node_t_info.count <= 1)
   {
    sprintf(line1,"Left Button:   Exit \"d.distance\" program.                                      ");
    sprintf(line2,"Middle Button: Exit \"d.distance\" program.                                      ");
    sprintf(line3,"Right Button:  Start all over.                                                 ");
   }
  else
   {
    sprintf(line1,"Left Button:   Print out measurements then go to menu to Exit or Extract.      ");
    sprintf(line2,"Middle Button: Do NOT print out measurements; go to menu to Exit or Extract.   ");
    sprintf(line3,"Right Button:  Start all over.                                                 ");
   }
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
  R_get_location_with_pointer(&screen_x, &screen_y, &button);
  if ((node_t_info.count<=1)&&(button==1))
    button = 2;  
  if (button == 1)
   {
/* Print out node to node distances.                                 */
    pipe_ptr = popen(PRT_COMMAND,"w");
    fprintf(pipe_ptr,"VECTOR FILE NAME:  %s\n",name );
    fprintf(pipe_ptr,"LOCATION:  %s\n",G_location() );
    fprintf(pipe_ptr,"MAPSET:    %s\n",G_mapset() );
    fprintf(pipe_ptr,"DATABASE:  %s\n",G_gisdbase() );
    fprintf(pipe_ptr,"Initial Pt.:    N%12.2lf E%11.2lf: Node: %4.4d\n",map->Node[ptr_node->node_number].y,map->Node[ptr_node->node_number].x,ptr_node->node_number);
    DPE_flag = 'P';
    N_dr_me_pr_e(map,p,DPE_flag);
    ptr_node_var = ptr_node + (node_t_info.count-1);
    fprintf(pipe_ptr,"Terminal Point: N%12.2lf E%11.2lf: Node: %4.4d\n",map->Node[ptr_node_var->node_number].y,map->Node[ptr_node_var->node_number].x,ptr_node_var->node_number);
    fprintf(pipe_ptr,"Initial Point to Terminal Point: (cummulative distance)       %17.2lfm\n",node_t_info.cum_dist);
    fprintf(pipe_ptr,"                                                               %16.2lff\n",(node_t_info.cum_dist*3.2808));
    fprintf(pipe_ptr,"                                                                 %14.2lfM\n",((node_t_info.cum_dist*3.2808)/5280.));
    fprintf(pipe_ptr,"Initial Point to Terminal Point: (shortest distance)          %17.2lfm\n",node_t_info.dir_dist);
    fprintf(pipe_ptr,"                                                               %16.2lff\n",(node_t_info.dir_dist*3.2808));
    fprintf(pipe_ptr,"                                                                 %14.2lfM\n",((node_t_info.dir_dist*3.2808)/5280.));
    fprintf(pipe_ptr,"\nNOTE:\n");
    fprintf(pipe_ptr,"n is node\n");
    fprintf(pipe_ptr,"m is meters\n");
    fprintf(pipe_ptr,"f is feet\n");
    fprintf(pipe_ptr,"M is miles\n");
    pclose(pipe_ptr);
    first_node_num = ptr_node->node_number;
    ptr_node_var = ptr_node + (node_t_info.count-1);
    last_node_num = ptr_node_var->node_number;
/* Draw plus at first node point.                                            */
    draw_PLUS(map,p,map->Node[first_node_num].y,map->Node[first_node_num].x,N2_COLOR);
/* Draw plus at last node point.                                             */
    draw_PLUS(map,p,map->Node[last_node_num].y,map->Node[last_node_num].x,N3_COLOR);
/* Only if "node_t_info.count" > 1 will you go to to "R".                    */
    if (node_t_info.count > 1)
      *next_letter = 'R';
    else
      *next_letter = 'z';
    return(1);
   }
  else
   {
    if (button == 2)
     {
/* Go to "R" only if "node_t_info.count" > 1.                                */
      if (node_t_info.count<=1)
        *next_letter = 'z';
      else
        *next_letter = 'R';
      return(1);
     }
    else
     {
      if (button == 3)
       {
/* Start over.                                                               */
        if (overlay_flag)
         {
          if (node_t_info.count>1)
           {
            first_node_num = ptr_node->node_number;
            ptr_node_var = ptr_node + (node_t_info.count-1);
            last_node_num = ptr_node_var->node_number;
            if ((node_t_info.count>2) &&
                (first_node_num!=last_node_num) )
             {
/* Erase plus at first node point.                                           */
              draw_PLUS(map,p,map->Node[first_node_num].y,map->Node[first_node_num].x,BG_COLOR);
/* Erase plus at last node point.                                            */
              draw_PLUS(map,p,map->Node[last_node_num].y,map->Node[last_node_num].x,BG_COLOR);
/* Erase "yellow" direct line distance from starting node to ending node.    */
              draw_seg(map,p,map->Node[first_node_num].y,map->Node[first_node_num].x,map->Node[last_node_num].y,map->Node[last_node_num].x,BG_COLOR);
             }
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
