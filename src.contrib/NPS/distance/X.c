/* X:  This function allows the user to either produce a short print out of  */
/* the measured line or a long print out.                                    */
#include "distance.h"
int
X(next_letter,map,p,name)
 char *next_letter;
 struct Map_info *map;
 struct line_pnts *p;
 char *name;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  char DSLE_flag;
  extern int term();
  extern int dr_me_pr_ex();
  extern int draw_PLUS();

#ifdef DEBUG
fprintf(stderr,"X\n");
#endif DEBUG
  sprintf(line1,"Left Button:   SHORT listing with distances for arcs but not segments.         ");
  sprintf(line2,"Middle Button: Return to previous menu.                                        ");
  sprintf(line3,"Right Button:  LONG listing with distances for arcs and segments.              ");
  sprintf(line4,"|  Initial Point  |  Terminal Point | Distance from Initial                   |"); 
  sprintf(line5,"|  \"red\" plus     |  \"violet\" plus  | to Node Point                           |");
  sprintf(line6,"| N %13.2f | N %13.2f | %14.3f meters (distance by arcs)|",initial.n,terminal.n,terminal.arc_dist_i_t);
  sprintf(line7,"| E %13.2f | E %13.2f | %14.3f meters (shortest dist.)  |",initial.e,terminal.e,terminal.dir_dist_i_t);
  strcpy(line8,"                                                                               ");
  term(line1,line2,line3,line4,line5,line6,line7,line8);
  R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
  if (button == 1)
   {
/* Produce "short" print out of measured line.                               */
    pipe_ptr = popen(PRT_COMMAND,"w");
    fprintf(pipe_ptr,"VECTOR FILE NAME:  %s\n",name );
    fprintf(pipe_ptr,"LOCATION:  %s\n",G_location() );
    fprintf(pipe_ptr,"MAPSET:    %s\n",G_mapset() );
    fprintf(pipe_ptr,"DATABASE:  %s\n",G_gisdbase() );
    fprintf(pipe_ptr,"Initial Pt.:N%12.2lf E%11.2lf\n",initial.n,initial.e);
    DSLE_flag = 'S';
    dr_me_pr_ex(map,p,DSLE_flag);
    fprintf(pipe_ptr,"Terminal Point:                        N%12.2lf E%11.2lf\n",terminal.n,terminal.e);
    fprintf(pipe_ptr,"Initial Point to Terminal Point: (distance by arcs)           %17.2lfm\n",terminal.arc_dist_i_t);
    fprintf(pipe_ptr,"                                                               %16.2lff\n",(terminal.arc_dist_i_t*3.2808));
    fprintf(pipe_ptr,"                                                                 %14.2lfM\n",((terminal.arc_dist_i_t*3.2808)/5280.));
    fprintf(pipe_ptr,"Initial Point to Terminal Point: (shortest distance)          %17.2lfm\n",terminal.dir_dist_i_t);
    fprintf(pipe_ptr,"                                                               %16.2lff\n",(terminal.dir_dist_i_t*3.2808));
    fprintf(pipe_ptr,"                                                                 %14.2lfM\n",((terminal.dir_dist_i_t*3.2808)/5280.));
    fprintf(pipe_ptr,"\nNOTE:\n");
    fprintf(pipe_ptr,"m is meters\n");
    fprintf(pipe_ptr,"f is feet\n");
    fprintf(pipe_ptr,"M is miles\n");
    fprintf(pipe_ptr,"PartArc is Partial Arc\n");
    pclose(pipe_ptr);
/* Draw "initial point" plus as "red".                                       */ 
    draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw a "violet" plus for the terminal point.                              */
    draw_PLUS(map,p,terminal.n,terminal.e,TP_COLOR);
    *next_letter = 'K';
    return(1);
   }
  else
   {
    if (button == 2)
     {
      *next_letter = 'Y';
      return(1);
     }
    else
     {
      if (button == 3)
       {
/* Produce "long" print out of measured line.                                */
        pipe_ptr = popen(PRT_COMMAND,"w");
        fprintf(pipe_ptr,"VECTOR FILE NAME:  %s\n",name );
        fprintf(pipe_ptr,"LOCATION:  %s\n",G_location() );
        fprintf(pipe_ptr,"MAPSET:    %s\n",G_mapset() );
        fprintf(pipe_ptr,"DATABASE:  %s\n",G_gisdbase() );
        fprintf(pipe_ptr,"Initial Pt.:N%12.2lf E%11.2lf\n",initial.n,initial.e);
        DSLE_flag = 'L';
        dr_me_pr_ex(map,p,DSLE_flag);
        fprintf(pipe_ptr,"Terminal Point:                        N%12.2lf E%11.2lf\n",terminal.n,terminal.e);
        fprintf(pipe_ptr,"Initial Point to Terminal Point: (distance by arcs)           %17.2lfm\n",terminal.arc_dist_i_t);
        fprintf(pipe_ptr,"                                                               %16.2lff\n",(terminal.arc_dist_i_t*3.2808));
        fprintf(pipe_ptr,"                                                                 %14.2lfM\n",((terminal.arc_dist_i_t*3.2808)/5280.));
        fprintf(pipe_ptr,"Initial Point to Terminal Point: (shortest distance)          %17.2lfm\n",terminal.dir_dist_i_t);
        fprintf(pipe_ptr,"                                                               %16.2lff\n",(terminal.dir_dist_i_t*3.2808));
        fprintf(pipe_ptr,"                                                                 %14.2lfM\n",((terminal.dir_dist_i_t*3.2808)/5280.));
        fprintf(pipe_ptr,"\nNOTE:\n");
        fprintf(pipe_ptr,"A is ARC\n");
        fprintf(pipe_ptr,"P is Partial SEGMENT\n");
        fprintf(pipe_ptr,"S is SEGMENT\n");
        fprintf(pipe_ptr,"m is meters\n");
        fprintf(pipe_ptr,"f is feet\n");
        fprintf(pipe_ptr,"M is miles\n");
        fprintf(pipe_ptr,"PartArc is Partial Arc\n");
        pclose(pipe_ptr);
/* Draw "initial point" plus as "red".                                       */
        draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw a "violet" plus for the terminal point.                              */
        draw_PLUS(map,p,terminal.n,terminal.e,TP_COLOR);
        *next_letter = 'K';
        return(1);
       }
     }
   }
 }
