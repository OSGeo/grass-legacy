/* D:  This function allows the user to "highlight/toggle" the possible arcs */
/* that you can go to next from the point you are presently at.  The         */ 
/* possible arcs are in "green" and the arc you can select is in "violet".   */
#include "distance.h"
int
D(next_letter,previous_letter,map,p)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 {
  int screen_x, screen_y ;
  int button;
  int hl_arc;
  extern int arc_in_WIND();
  extern int choose_arc();
  extern int chosen_arc();
  extern int erase_lm();
  extern int draw_PLUS();
  extern int term();
  extern int dr_me_pr_ex();
  extern int draw_ARC();
  int present_hl_arc;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  int hl_stripe;
  int i;
  char DSLE_flag;

#ifdef DEBUG
fprintf(stderr,"D\n");
#endif DEBUG
  sprintf(line1,"Left Button:   Highlight/toggle in \"violet\" each of the \"green\" arcs.          ");
  sprintf(line2,"Middle Button: Abort.                                                          ");
  sprintf(line3,"Right Button:  Select present \"violet\" arc as next arc.                        ");
  if (arc_t_info.count > 1)
   {
    sprintf(line4,"|  Initial Point  |  Node Point     | Distance from Initial                   |"); 
    sprintf(line5,"|  \"red\" plus     |  \"violet\" plus  | to Node Point                           |");
    sprintf(line6,"| N %13.2f | N %13.2f | %14.3f meters (distance by arcs)|",initial.n,last_m_pt.n,last_m_pt.arc_dist_i_lm);
    sprintf(line7,"| E %13.2f | E %13.2f | %14.3f meters (shortest dist.)  |",initial.e,last_m_pt.e,last_m_pt.dir_dist_i_lm);                 
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
  term(line1,line2,line3,line4,line5,line6,line7,line8);
/* Read present arc.                                                         */
  dig_P_read_line(map,(abs(present.arc)),&p);
  hl_arc = present.arc;
  present_hl_arc = hl_arc;
  choose_arc(map,p,&hl_arc);
/* Draw "initial point" as "red" plus.                                       */ 
  draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
  while (1)
   {
    R_get_location_with_pointer(&screen_x, &screen_y, &button);
    if (button == 1)
     {
/* Highlight/toggle in violet each of the possible green arcs you can go to. */
      *previous_letter = 'D';
      *next_letter = 'D';
      present_hl_arc = hl_arc;
      choose_arc(map,p,&hl_arc);
/* Draw next "initial point" plus as "red".                                  */
      draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
      if (arc_t_info.count > 1)
       {
/* Draw next "present point" plus as "white".                                */ 
        draw_PLUS(map,p,present.n,present.e,PP_COLOR);
/* Draw "last measured point" plus as "violet".                              */
        draw_PLUS(map,p,last_m_pt.n,last_m_pt.e,LM_COLOR);
       }
     }
    else
     {
      if (button == 2)
       {
        *previous_letter = 'D';
        *next_letter = 'Z';
        return(1);
       }
      else
       {
        if (button == 3)
          {
/* Select the present "violet" arcs as the next arc to go to.                */

/* The value "present_hl_arc" is the next arc to go to...                    */
/* At this point you have selected the next arc and the node point you       */
/* are going to start from...                                                */
/* You must now load information into "arc_table" structure...               */
/* You must now update the "present" structure for the new arc.              */
/* This includes the "present" point.                                        */
/* You must calculate arc distance "arc_dist_i_lm" and direct distance       */
/* "dir_dist_i_lm".                                                          */

/* If value for "arc_t_info.count" is equal to or greater than the value     */
/* for "arc_t_info.total" then increase the memory capacity of the           */
/* structure "ARC_TABLE" by 10.                                              */
          if (arc_t_info.count>=arc_t_info.total)
           {
            arc_t_info.total += 10;
            ptr_arc=(struct ARC_TABLE *) realloc (ptr_arc,(sizeof(struct ARC_TABLE)*arc_t_info.total));
            if (ptr_arc == NULL)
             {
              sprintf(line1,"OUT OF MEMORY using realloc for \"ptr_arc\" pointer.                             ");
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
/* Set correct pointer position for "arc_table" structure.                   */
          ptr_arc_var = ptr_arc + arc_t_info.count;
/* Assign value of present_hl_arc" to "arc_table.arc".                       */
          ptr_arc_var->arc = present_hl_arc;
/* Erase present point plus ("white plus") and all the adjoining segments.   */
          erase_pp(map,p);
/* Assign "present_hl_arc" value to "initial.arc" if "arc_t_info.count" = 0  */
          if (arc_t_info.count == 0)
           {
            initial.arc = present_hl_arc;
            if (((initial.indicator=='1')&&(initial.arc<0)) ||
                ((initial.indicator=='2')&&(initial.arc>0))   )
             {
/* If initial.indicator = 1 then initial arc must be going in a negative     */
/* direction.                                                                */
/* If initial.indicator = 2 then initial arc must be going in a positive     */
/* direction.                                                                */
              if (present_hl_arc>0)
               {
/* "present_hl_arc is going in a positive direction.                         */
/* Set values in the "initial" structure.                                    */
                initial.indicator='1';
                initial.arc = present_hl_arc;
                initial.node = map->Line[abs(present_hl_arc)].N1;
                initial.segment = 1;
               }
              else
               {
/* "present_hl_arc is going in a negative direction.                         */
/* Set values in the "initial" structure.                                    */
                initial.indicator='2';
                initial.arc = present_hl_arc;
                initial.node = map->Line[abs(present_hl_arc)].N2;
                dig_P_read_line(map,(abs(initial.arc)),&p);
                initial.segment = (p->n_points-1);
               }
             }
           }
          else 
           {
/* Erase last measured point plus "violet plus".                             */
            erase_lm(map,p);
           }
/* Draw "violet/green" segments for the "violet" arc                         */
/* that you selected to be the next arc to work on.                          */
/* "present_hl_arc" is the violet arc that you selected.                     */
/* Draw "violet/green" arc as the chosen arc.                                */
          chosen_arc(map,p,present_hl_arc);
/* Assign values in "last_m_pt" and "present" structures.                    */
          if (present_hl_arc > 0)
           {
/* "present_hl_arc" is going in a positive direction.                        */
/* Set "last_m_pt" structure values.                                         */
/* This "last_m_pt" point will be at the beginning of the "present_hl_arc"   */
/* and since you are going in a positive direction then the node N1 will be  */
/* that point.                                                               */
            last_m_pt.n = map->Node[map->Line[abs(present_hl_arc)].N1].y;
            last_m_pt.e = map->Node[map->Line[abs(present_hl_arc)].N1].x;
            last_m_pt.segment = 1;
            last_m_pt.indicator = 'a';
            last_m_pt.node = map->Line[abs(present_hl_arc)].N1;
            last_m_pt.arc = present_hl_arc;
/* Set new "present" structure values.                                       */
/* This new "present" point will be at the end of the "present_hl_arc" and   */
/* since you are going in a positive direction then the node N2 will be      */ 
/* that  point.                                                              */
            present.n = map->Node[map->Line[abs(present_hl_arc)].N2].y;
            present.e = map->Node[map->Line[abs(present_hl_arc)].N2].x;
            present.segment = (p->n_points-1);
            present.indicator = 'b';
            present.node = map->Line[abs(present_hl_arc)].N2;
/* Switch the direction of "present" arc so that you will be going in the    */
/* opposite direction of the "present_hl_arc".                               */
            present.arc = -(present_hl_arc);
           }
          else
           {
/* "present_hl_arc" is going in a negative direction.                        */
/* Set "last_m_pt" structure values.                                         */
/* This "last_m_pt" point will be at the end  of the "present_hl_arc"        */
/* and since you are going in a negative direction then the node N2 will be  */
/* that point.                                                               */
            last_m_pt.n = map->Node[map->Line[abs(present_hl_arc)].N2].y;
            last_m_pt.e = map->Node[map->Line[abs(present_hl_arc)].N2].x;
            last_m_pt.segment = (p->n_points-1);
            last_m_pt.indicator = 'b';
            last_m_pt.node = map->Line[abs(present_hl_arc)].N2;
            last_m_pt.arc = present_hl_arc;
/* Set new "present" structure values.                                       */
/* This new "present" point will be at the beginning of the "present_hl_arc" */
/* and since you are going in a negative direction then the node N1 will be  */ 
/* that  point.                                                              */
            present.n = map->Node[map->Line[abs(present_hl_arc)].N1].y;
            present.e = map->Node[map->Line[abs(present_hl_arc)].N1].x;
            present.segment = 1;
            present.indicator = 'a';
            present.node = map->Line[abs(present_hl_arc)].N1;
/* Switch the direction of "present" arc so that you will be going in the   */
/* opposite direction of the "present_hl_arc".                              */
            present.arc = -(present_hl_arc);
           }
/* If "arc_t_info.count" > 0 then draw the "red" measured line from the     */
/* "initial point" to the "last measured point".                            */
          if (arc_t_info.count > 0)
           {
            DSLE_flag = 'D';
            dr_me_pr_ex(map,p,DSLE_flag);
           }
/* If you are beyond the "initial.arc" and the "present.arc" (which is the    */
/* previous arc) is the same as the "present_hl_arc" (which is the high-      */
/* lighted "violet" arc) then redraw highlighted arc so that the              */
/* "green/violet" will draw over the measured "red" line.                     */
          if ( (arc_t_info.count>0)&&(abs(present_hl_arc)==abs(present.arc) ) )
           {
/* Draw "green/violet" segments over measured "red" line.                     */
              draw_ARC(map,p,present_hl_arc,(int)1,HL_COLOR1,HL_COLOR2);
           }
/* Draw "initial point" as "red" plus.                                       */ 
          draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw next "present point" plus as "white".                                */ 
          draw_PLUS(map,p,present.n,present.e,PP_COLOR);
          if (arc_t_info.count > 0)
           {
/* Draw "last measured point" plus as "violet".                              */
            draw_PLUS(map,p,last_m_pt.n,last_m_pt.e,LM_COLOR);
           }
/* Increment "arc_t_info.count" by 1.                                        */
          arc_t_info.count += 1;
/* Determine if "present_hl_arc" arc is within the "WIND" area.              */
          if ( arc_in_WIND(map,p,((int)abs(present_hl_arc))) )
           {
            *previous_letter = 'D';
/* "present_hl_arc" is entirely within the "WIND" area so go to "H".         */
            *next_letter = 'H';
           }
          else
           {
/* The arc "present_hl_arc" is not entirely within the "WIND" area.          */
/* Determine if point (present.n,present.e) is within the "WIND" area.       */
            if ( pt_in_WIND(present.n,present.e) )
             {
/* Point(present.n,present.e) is within the "WIND" area so go to "H".        */
              *previous_letter = 'D';
              *next_letter = 'H';  
             }
            else
             {
/* Point(present.n,present.e) is not within the "WIND" area so go to "J".    */
              *previous_letter = 'D';
              *next_letter = 'J';  
             }
           }
          return(1);
         }
       }
     }
   }
 }
