#include "distance.h"
int
R(next_letter,map,p,overlay_flag,name,mapset)
 char *next_letter;
 struct Map_info *map;
 struct line_pnts *p;
 int overlay_flag;
 char *name;
 char *mapset;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  char asc_name[20];
  char bin_name[20];
  char DPE_flag;
  int first_node_num, last_node_num;
  extern int rd_dg_bn_hd();
  extern int dig_asc_file();
  extern int dig_bin_file();
  extern int prompt4asc();
  extern int prompt4bin();
  extern int wr_dg_hd_bn();
  extern int wr_dg_bn_pts();
  extern int N_dr_me_pr_e();
  extern int draw_PLUS();
  extern int draw_seg();
  extern int erase_node();
  extern int term();
  int itype;
  char ctype;

#ifdef DEBUG
fprintf(stderr,"R\n");
#endif DEBUG
  sprintf(line1,"Left Button:   Extract measured line and produce digit file from that line.    ");
  sprintf(line2,"Middle Button: Did not want to extract measured line.  Exit program.           ");
  sprintf(line3,"Right Button:  Did not want to extract measured line.  Start all over.         ");
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
  if (button == 1)
   {
    if (node_t_info.count > 1)
     {
/* Read digit binary header.                                                 */
      if (rd_dg_bn_hd(name,mapset))
       {
/* Clear the screen.                                                         */
#ifndef NO_CURSES
        clear(); /* SAVE this line!!! */
#endif NO_CURSES
/* End the "curses" with "endwin" function.                                  */
#ifndef NO_CURSES
        endwin(); /* SAVE this line!!! */
#endif NO_CURSES
#ifdef ASCII
/* Create "ASCII" file "dig_asc".                                           */
/* Prompt for what the user wants the new ascii digit file to be called.     */
/* (use Vask library routines) */
        if (prompt4asc(asc_name)!=1)
         {
          return(0);
         }
/* Determine whether "ctype" is "L" or "P".                                  */
        if (total_points == 2)
         {
          first_node_num = ptr_node->node_number;
          ptr_node_var = ptr_node + (node_t_info.count-1);
          last_node_num = ptr_node_var->node_number;
          if ( (map->Node[first_node_num].y==map->Node[last_node_num].y) &&
               (map->Node[first_node_num].x==map->Node[last_node_num].x)   )
           {
            ctype = 'P'; 
           }
          else
           {
            ctype = 'L'; 
           }
         }
        else
         {
          ctype = 'L'; 
         }
        if (dig_asc_file(asc_name,ctype)!=1)
         {
          return(0);
         }
/* Extract line here.                                                        */
        DPE_flag = 'E';
        N_dr_me_pr_e(map,p,DPE_flag);
        first_node_num = ptr_node->node_number;
        ptr_node_var = ptr_node + (node_t_info.count-1);
        last_node_num = ptr_node_var->node_number;
/* Draw plus at first node point.                                            */
        draw_PLUS(map,p,map->Node[first_node_num].y,map->Node[first_node_num].x,N2_COLOR);
/* Draw plus at last node point.                                             */
        draw_PLUS(map,p,map->Node[last_node_num].y,map->Node[last_node_num].x,N3_COLOR);
/* Write last point to "dig_asc".                                            */
        fprintf(dig_asc," %12.2lf %12.2lf\n",map->Node[last_node_num].y,map->Node[last_node_num].x);
/* Close digit ascii file (dig_asc).                                         */
        fclose(dig_asc);
        END(map);
        *next_letter = 'e';
/* Send message to user about the new digit ascii file created.              */
        fprintf(stderr,"\nATTENTION:  Execute \"a.b.vect\" to convert the ascii file:\n\"%s\" to a binary digit file.\n",asc_name);
#else
/* Create "BINARY" file "dig_bin".                                           */
/* Prompt for what the user wants the new binary digit file to be called.    */
/* (use Vask library routines) */
        if (prompt4bin(bin_name)!=1)
         {
          return(0);
         }
        if (dig_bin_file(bin_name)!=1)
         {
          return(0);
         }
/* Assign memory space for pointers "*x_array" and "*y_array".               */
        x_array = (double *) malloc (sizeof(double)*total_points);
        if (x_array == NULL)
         {
          fprintf(fprintf,"\nOUT OF MEMORY using malloc for \"x_array\" pointer.\n");
          fclose(dig_bin);
          return(0);
         }
        y_array = (double *) malloc (sizeof(double)*total_points);
        if (y_array == NULL)
         {
          fprintf(fprintf,"\nOUT OF MEMORY using malloc for \"y_array\" pointer.\n");
          fclose(dig_bin);
          return(0);
         }
/* Write header information to binary digit file.                            */
        wr_dg_hd_bn();
/* Extract line here.                                                        */
        x_counter = 0;
        y_counter = 0;
        DPE_flag = 'E';
        N_dr_me_pr_e(map,p,DPE_flag);
        first_node_num = ptr_node->node_number;
        ptr_node_var = ptr_node + (node_t_info.count-1);
        last_node_num = ptr_node_var->node_number;
/* Draw plus at first node point.                                            */
        draw_PLUS(map,p,map->Node[first_node_num].y,map->Node[first_node_num].x,N2_COLOR);
/* Draw plus at last node point.                                             */
        draw_PLUS(map,p,map->Node[last_node_num].y,map->Node[last_node_num].x,N3_COLOR);
/* Assign last point of measured line to "x_array" and "y_array".            */
        *(x_array+x_counter) = map->Node[last_node_num].x;
        x_counter += 1;
        *(y_array+y_counter) = map->Node[last_node_num].y;
        y_counter += 1;
/* Determine whether points in "x_array" and "y_array"  are a LINE or POINT. */
        if (total_points==2)
         {
          if ( (*(x_array+0) == *(x_array+1)) &&
               (*(y_array+0) == *(y_array+1))    )
           {
            itype = FILE_DOT;
           }
          else
           {
            itype = FILE_LINE;
           }
         }
        else
         {
          itype = FILE_LINE;
         }
/* Write the entire "x_array" and "y_array" to file "dig_bin".               */
        wr_dg_bn_pts(itype);
/* Close digit binary file (dig_bin).                                        */
        fclose(dig_bin);
        END(map);
        *next_letter = 'e';
/* Send message to user about the new digit binary file created.             */
        fprintf(stderr,"\nATTENTION:  Execute \"support.vect\" on the new binary digit file:\n\"%s\".\n",bin_name);
#endif ASCII
       }
      else
       {
        *next_letter = 'z';
       }
     }
    else
     {
      *next_letter = 'z';
     }
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
         {
          if (node_t_info.count>1)
           {
            first_node_num = ptr_node->node_number;
            ptr_node_var = ptr_node + (node_t_info.count-1);
            last_node_num = ptr_node_var->node_number;
            if ((node_t_info.count>2)&&
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
