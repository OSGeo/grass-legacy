/* F:  In this function, you will determine whether to measure on the        */
/* existing arcs (button 1) or measure from node to node (button 3).         */
#include "distance.h"
int
F(next_letter,previous_letter,map,p,arcs_drawn,nodes_drawn)
 char *next_letter;
 char *previous_letter;
 struct Map_info *map;
 struct line_pnts *p;
 int arcs_drawn, nodes_drawn;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  extern int term();

#ifdef DEBUG
fprintf(stderr,"F\n");
#endif DEBUG
/* There have been no arcs on nodes drawn with the "WIND" area.              */
  if ((arcs_drawn==0)&&(nodes_drawn==0))
    return(0);
  if (arcs_drawn==1)
   {
/* You can only measure on existing arcs if there are some existing arcs.    */
/* "arcs_drawn" will equal 1 if there are some existing arcs.                */
    if (nodes_drawn==1)
     {
/* "arcs_drawn" equals 1 and "nodes_drawn" equals 1.                         */
      sprintf(line1,"Left Button:   Measure arc distances from and to any point on existing arcs.   ");
      sprintf(line2,"Middle Button: Exit \"d.distance\" program.                                      ");
      sprintf(line3,"Right Button:  Measure direct distances from existing node point to node point.");
     }
    else
     {
/* "arcs_drawn" equals 1 and "nodes_drawn" equals 0.                         */
      sprintf(line1,"Left Button:   Measure arc distances from and to any point on existing arcs.   ");
      sprintf(line2,"Middle Button: Exit \"d.distance\" program.                                      ");
      sprintf(line3,"Right Button:  Measure arc distances from and to any point on existing arcs.   ");
     }
   }
  else
   {
/* "arcs_drawn" equals 0 and "nodes_drawn" equals 1.                         */
      sprintf(line1,"Left Button:   Measure arc distances from and to any point on existing arcs.   ");
    sprintf(line1,"Left Button:   Measure direct distances from existing node point to node point.");
    sprintf(line2,"Middle Button: Exit \"d.distance\" program.                                      ");
    sprintf(line3,"Right Button:  Measure direct distances from existing node point to node point.");
   }
/* Provide the user information on how to change the scale.                  */ 
  strcpy(line4,"NOTE: You may want to enlarge the scale if segments are too small to discern.  ");
  strcpy(line5,"You can enlarge the scale of the area by using \"d.window\" or \"window\".         ");
  strcpy(line6,"If this vector file has been newly created or modified by \"digit\" then run     ");
  strcpy(line7,"\"support.vect\" (only once) on the vector file prior to running \"d.distance\".   "); 
  strcpy(line8,"                                                                               ");
  term(line1,line2,line3,line4,line5,line6,line7,line8);
  R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
/* You must measure "existing arcs" since there were no nodes drawn.         */ 
  if ((arcs_drawn==1)&&(nodes_drawn==0)&&(button==3))
    button = 1;
/* You must measure "existing nodes" since there were no arcs drawn.         */
  if ((arcs_drawn==0)&&(nodes_drawn==1)&&(button==1))
    button = 3;
  if (button == 1)
   {
/* Measure "existing arcs".                                                  */
/* "ptr_arc" will only be equal to NULL at the very beginning of running the */
/* program.                                                                  */
    if (ptr_arc == NULL)
     {
/* Set up arc pointers */
/* "arc_t_info.total" is how many arcs you can initially use or have         */
/* storage space for.                                                        */
      arc_t_info.total = TOTAL_ARCS;
ptr_arc=(struct ARC_TABLE *) malloc ((sizeof(struct ARC_TABLE)*arc_t_info.total));
      if (ptr_arc == NULL)
       {
        sprintf(line1,"OUT OF MEMORY using malloc for \"ptr_arc\" pointer.                              ");
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
      arc_t_info.addr_first_arc = ptr_arc;
      arc_t_info.addr_last_arc  = NULL;
     }
/* Initialize all variable for measuring existing arcs.                      */
/* Clear out all variables in "present" structure.                           */
    present.n   = 0.0;
    present.e   = 0.0;
    present.arc = 0;
    present.segment = 0;
    present.indicator = '0';
    present.node = 0;
/* Clear out all variables for "initial" structure.                          */
    initial.n   = 0.0;
    initial.e   = 0.0;
    initial.arc = 0;
    initial.segment = 0;
    initial.indicator = '0';
    initial.node = 0;
/* Clear out all variables for "terminal" structure.                         */
    terminal.n   = 0.0;
    terminal.e   = 0.0;
    terminal.arc = 0;
    terminal.segment = 0;
    terminal.indicator = '0';
    terminal.node = 0;
    terminal.arc_dist_i_t = 0.0;
    terminal.dir_dist_i_t = 0.0;
/* Clear out all variables for "last_m_pt" structure.                        */
    last_m_pt.n   = 0.0;
    last_m_pt.e   = 0.0;
    last_m_pt.arc = 0;
    last_m_pt.segment = 0;
    last_m_pt.indicator = '0';
    last_m_pt.node = 0;
    last_m_pt.arc_dist_i_lm = 0.0;
    last_m_pt.dir_dist_i_lm = 0.0;
/* Set value to zero for "arc_t_info.count" variable.                        */
    arc_t_info.count = 0;
/* Set value of arc_t_info.addr_last_arc to NULL.                            */
    arc_t_info.addr_last_arc = NULL;
    *previous_letter = 'F';
    *next_letter = 'A';
   }
  else
   {
    if (button == 2)
     {
      *previous_letter = 'F';
      *next_letter = 'z';
     }
    else
     {
      if (button == 3)
       {
/* Measure "existing nodes".                                                 */
        if (ptr_node == NULL)
         {
/* Set up node pointers.                                                     */
/* "node_t_info.total" is how many nodes you can initially use or have       */
/* storage space for.                                                        */
          node_t_info.total = TOTAL_NODES;
ptr_node=(struct NODE_TABLE *) malloc ((sizeof(struct NODE_TABLE)*node_t_info.total));
          if (ptr_node == NULL)
           {
            sprintf(line1,"OUT OF MEMORY using malloc for \"ptr_node\" pointer.                             ");
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
/* Initialize necessary variables for measuring direct distances.            */
/* Set value to zero for "node_t_info.count" variable.                       */
        node_t_info.count = 0;
        *previous_letter = 'F';
        *next_letter = 'I';
       }
     }
   }
  return(1);
 }
