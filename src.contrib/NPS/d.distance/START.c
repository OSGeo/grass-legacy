#include "distance.h"
int
START(map,p,mapset,name,next_letter,overlay_flag,arcs_drawn,nodes_drawn)
 struct Map_info *map;
 struct line_pnts *p;
 char *mapset;
 char *name;
 char *next_letter;
 int overlay_flag;
 int *arcs_drawn;
 int *nodes_drawn;
 {
  int arc;
  extern draw_ARC();
  int stripe;
  int node;
  int number_of_arcs;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  extern int setup();
  extern int draw_ARC();
  extern int draw_NODE();
  extern int draw_WIND();

#ifdef DEBUG
fprintf(stderr,"a (START)\n");
#endif DEBUG
  if (  ! (setup(map,p,mapset,name,&number_of_arcs,overlay_flag)) )
    return(0);
/* Set variable "stripe" to stripe arc lines.                                */
  stripe = 1;
  *arcs_drawn = 0;
  if (number_of_arcs > 0)
   {
/*  Draw all arcs for "map" with striping.                                   */
    for (arc=1; arc <= number_of_arcs; arc++)
     {
      dig_P_read_line(map,arc,&p);
      if ( draw_ARC(map,p,arc,stripe,IA_COLOR1,IA_COLOR2) )
        *arcs_drawn = 1;
     }
   }
  *nodes_drawn = 0;
/* Draw all nodes.                                                           */
  if ( map->n_nodes > 0)
   {
    for (node=1; node <= map->n_nodes; node++)
     {
      if (map->Node[node].alive != 0)
       {
        if (   ((map->Node[node].y >= window.south) 
            &&  (map->Node[node].y <= window.north)) 
            && ((map->Node[node].x >= window.west)
            &&  (map->Node[node].x <= window.east) ) )
         {
          if (draw_NODE(map,p,node,IN_COLOR) )
            *nodes_drawn = 1;
         }
       }
     }
   }
/* Draw boundary of WIND.                                                    */
  draw_WIND(map,p);
/* If there were any arcs on nodes drawn in "WIND" area then run program.    */
  if (((*arcs_drawn==1)&&(*nodes_drawn==1))||
      ((*arcs_drawn==1)&&(*nodes_drawn==0))||
      ((*arcs_drawn==0)&&(*nodes_drawn==1))  )
   {
    *next_letter = 'F';
    return(1);
   }
  else
   {
/* If there weren't any arcs on nodes drawn in "WIND" area then end program. */
    strcpy(line1,"                                                                               ");
    sprintf(line2,"There are no arcs or nodes within the \"window\" area.                           ");
    strcpy(line3,"                                                                               ");
    strcpy(line4,"                                                                               ");
    strcpy(line5,"                                                                               ");
    strcpy(line6,"                                                                               ");
    strcpy(line7,"                                                                               ");
    strcpy(line8,"                                                                               ");
    error_msg(line1,line2,line3,line4,line5,line6,line7,line8);
    *next_letter = 'z';
    return(0);
   }
 }
