/* "dr_me_pr_ex" function will draw, measure, print, or extract the line     */
/* you have measured.  The value in variable "DSLE_flag" will determine      */
/* which operation that this function will perform.                          */
/* If DSLE_flag = 'D' then this function will draw and measure your measured */
/* line.                                                                     */
/* If DSLE_flag = 'L' then this function will draw, measure, and print a     */
/* long output listing of the measured line with arc and segment distances.  */
/* If DSLE_flag = 'S' then this function will draw, measure, and print a     */
/* short output listing of the measured line with arc distances.             */
/* If DSLE_flag = 'E' then this function will draw, measure, and extract and */
/* create an ascii digit file of the line measured.                          */

/* "indicator" for PRESENT, INITIAL, TERMINAL, and LAST_M_PT structures are  */
/* defined as follows:                                                       */
/*                                                                           */
/* Indicator | Definition                                                    */
/*           |                                                               */
/*     1     | I & N (N1)                                                    */
/*     2     | I & N (N2)                                                    */
/*     3     | I & B                                                         */
/*     4     | I & E                                                         */
/*     5     | I & M                                                         */
/*           |                                                               */
/*     6     | T & N (N1)                                                    */
/*     7     | T & N (N2)                                                    */
/*     8     | T & B                                                         */
/*     9     | T & E                                                         */
/*     0     | T & M                                                         */
/*                                                                           */
/* NOTE: I is initial point (first point for measured line)                  */
/*       N is node point                                                     */
/*       B is beginning point of a segment                                   */
/*       E is ending point of a segment                                      */
/*       M is middle point of a segment and is not either of the end points  */
/*       T is terminal point (last point for measured line)                  */
/*       N1 is the beginning node number for an arc                          */
/*       N2 is the ending node number for an arc                             */


#include "distance.h"
int
dr_me_pr_ex(map,p,DSLE_flag)
 struct Map_info *map;
 struct line_pnts *p;
 char DSLE_flag;
 {
  extern int draw_seg();
  int i;
  extern int draw_seg();
  extern int draw_SEGMENT();
  extern int draw_ARC();
  int arc_num;
  int arc_num2;
  struct ARC_TABLE *ptr_a_begin, *ptr_a_end;
  struct ARC_TABLE *ptr_a_begin2, *ptr_a_end2, *ptr_arc_var2;
  double dist;
  double term_arc_dist;

#ifdef DEBUG
fprintf(stderr,"dr_me_pr_ex\n");
#endif DEBUG
/* Set "arc_dist_i_lm" and "dir_dist_i_lm" in "last_m_pt" structure to 0 */
  last_m_pt.arc_dist_i_lm = 0.0;
  last_m_pt.dir_dist_i_lm = 0.0;
/* Set "arc_dist_i_t" and "dir_dist_i_t" in "terminal" structure to 0 */
  terminal.arc_dist_i_t = 0.0;
  terminal.dir_dist_i_t = 0.0;
#ifdef DEBUG_dmpe
fprintf(stderr,"\ndr_me_pr_ex:  initial.indicator=%c\n",initial.indicator);
fprintf(stderr,  "dr_me_pr_ex:  initial.arc=%d\n",initial.arc);
fprintf(stderr,  "dr_me_pr_ex:  initial.segment=%d\n",initial.segment);
fprintf(stderr,  "dr_me_pr_ex:  initial.node=%d\n",initial.node);
fprintf(stderr,  "dr_me_pr_ex:  terminal.indicator=%c\n",terminal.indicator);
fprintf(stderr,  "dr_me_pr_ex:  terminal.arc=%d\n",terminal.arc);
fprintf(stderr,  "dr_me_pr_ex:  terminal.segment=%d\n",terminal.segment);
fprintf(stderr,  "dr_me_pr_ex:  terminal.node=%d\n\n",terminal.node);
fprintf(stderr,  "dr_me_pr_ex:  last_m_pt.indicator=%c\n",last_m_pt.indicator);
fprintf(stderr,  "dr_me_pr_ex:  last_m_pt.arc=%d\n",last_m_pt.arc);
fprintf(stderr,  "dr_me_pr_ex:  last_m_pt.segment=%d\n",last_m_pt.segment);
fprintf(stderr,  "dr_me_pr_ex:  last_m_pt.node=%d\n\n",last_m_pt.node);
#endif DEBUG_dmpe
  ptr_a_begin = ptr_arc;
  ptr_a_end   = ptr_arc + (arc_t_info.count-1);
  for (ptr_arc_var=ptr_a_begin; ptr_arc_var <= ptr_a_end; ptr_arc_var++)
   {
    arc_num = ptr_arc_var->arc;
#ifdef DEBUG_dmpe
fprintf(stderr,"dr_me_pr_ex: >>>>>> arc_num=%d\n",arc_num);  
#endif DEBUG_dmpe
    dig_P_read_line(map,abs(arc_num),&p);
/* If the "first" and "last" arc are the same */
    if ( (ptr_arc_var == arc_t_info.addr_first_arc) &&
         (ptr_arc_var == arc_t_info.addr_last_arc)     )
     {

/* THE FIRST ARC AND THE LAST ARC ARE THE SAME AND THERE IS ONLY 1 ARC.      */ 

#ifdef DEBUG_dmpe
fprintf(stderr,"dr_me_pr_ex:  Draw in \"red\" only 1 arc;  first and last arcs are the same;  arc is %d.\n",arc_num);
#endif DEBUG_dmpe
/* Redraw the terminal arc as the initial colors */
      draw_ARC(map,p,terminal.arc,(int)1,IA_COLOR1,IA_COLOR2);
      only_1_arc(map,p,DSLE_flag);
     }
    else
     {
/* if this is the "first" arc (position 0 in arc_table) */
      if (ptr_arc_var == arc_t_info.addr_first_arc)
       {

/* THIS IS THE INITIAL ARC */

#ifdef DEBUG_dmpe
fprintf(stderr,"dr_me_pr_ex:  Draw in \"red\" the initial arc;  arc is %d.\n",arc_num);
#endif DEBUG_dmpe

/* at 0 position is the "arc_table" structure */
/* draw portion of initial arc */
/* initial point */
        if (initial.indicator=='5')
         {
/* "point" is a mid-point of a segment as well as the initial point */
/* "point" is NOT an end point of a segment and is NOT a node */
/* Read initial arc */
          dig_P_read_line(map,abs(initial.arc),&p);
          if (initial.arc > 0)
           {
/* Initial arc is going in a positive direction */

/* Erase partial segment */
            draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],BG_COLOR);
/* Draw partial segment in "red" */
            draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],MA_COLOR);
/* Calculate distance of "red" partial segment */
            dist_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],initial.arc,initial.segment,DSLE_flag,&dist);
            last_m_pt.arc_dist_i_lm += dist;
            terminal.arc_dist_i_t   += dist;
            for (i=(initial.segment+1); i < p->n_points; i++)
             {
/* Erase segment */
              draw_SEGMENT(map,p,initial.arc,i,BG_COLOR);
/* Draw segment "red" segment */
              draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
              dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
              last_m_pt.arc_dist_i_lm += dist;
              terminal.arc_dist_i_t   += dist;
             }
            if (DSLE_flag=='S'||DSLE_flag=='L')
             {
              fprintf(pipe_ptr,"PartArc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(initial.arc),initial.n,initial.e,p->y[(p->n_points-1)],p->x[(p->n_points-1)],terminal.arc_dist_i_t);
              fprintf(pipe_ptr,"                                                                 %14.2lff\n",(terminal.arc_dist_i_t*3.2808));
             }
           }
          else
           {
/* Initial arc is going in a negative direction */
/* Erase partial segment */
            draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],BG_COLOR);
/* Draw "red" partial segment */
            draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],MA_COLOR);
/* Calculate distance of "red" partial segment */
            dist_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],initial.arc,initial.segment,DSLE_flag,&dist);
            last_m_pt.arc_dist_i_lm += dist;
            terminal.arc_dist_i_t   += dist;
            for (i=(initial.segment-1); i > 0; i--)
             {
/* Erase segment */
              draw_SEGMENT(map,p,initial.arc,i,BG_COLOR);
/* Draw "red" segment */
              draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
              dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
              last_m_pt.arc_dist_i_lm += dist;
              terminal.arc_dist_i_t   += dist;
             }
            if (DSLE_flag=='S'||DSLE_flag=='L')
             {
              fprintf(pipe_ptr,"PartArc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(initial.arc),initial.n,initial.e,p->y[0],p->x[0],terminal.arc_dist_i_t);
              fprintf(pipe_ptr,"                                                                 %14.2lff\n",(terminal.arc_dist_i_t*3.2808));
             }
           }
/* Redraw two end nodes for initial.arc */
          draw_NODE(map,p,map->Line[abs(initial.arc)].N1,IN_COLOR);
          draw_NODE(map,p,map->Line[abs(initial.arc)].N2,IN_COLOR);
         }
        else
         {
          if (initial.indicator=='3')
           {
/* "point" is the initial point as well as the beginning */
/* point of a segment but NOT a node point of an arc */
/* Read initial arc */
            dig_P_read_line(map,(abs(initial.arc)),&p);
            if (initial.arc>0)
             {
/* Initial arc is going in a positive direction */
              for (i=initial.segment; i < p->n_points; i++) 
               {
/* Erase segment */
                draw_SEGMENT(map,p,initial.arc,i,BG_COLOR);
/* Draw "red" segment */
                draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                last_m_pt.arc_dist_i_lm += dist;
                terminal.arc_dist_i_t   += dist;
               }
              if (DSLE_flag=='S'||DSLE_flag=='L')
               {
                fprintf(pipe_ptr,"PartArc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(initial.arc),initial.n,initial.e,p->y[(p->n_points-1)],p->x[(p->n_points-1)],terminal.arc_dist_i_t);
                fprintf(pipe_ptr,"                                                                 %14.2lff\n",(terminal.arc_dist_i_t*3.2808));
               }
             }
            else
             {
/* Initial arc is going in a negative direction */
              for (i=(initial.segment-1); i > 0; i--)
               {
/* Erase segment */
                draw_SEGMENT(map,p,initial.arc,i,BG_COLOR);
/* Draw "red" segment */
                draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                last_m_pt.arc_dist_i_lm += dist;
                terminal.arc_dist_i_t   += dist;
               }
              if (DSLE_flag=='S'||DSLE_flag=='L')
               {
                fprintf(pipe_ptr,"PartArc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(initial.arc),initial.n,initial.e,p->y[0],p->x[0],terminal.arc_dist_i_t);
                fprintf(pipe_ptr,"                                                                 %14.2lff\n",(terminal.arc_dist_i_t*3.2808));
               }
             }
/* Redraw two end nodes for initial.arc */
            draw_NODE(map,p,map->Line[abs(initial.arc)].N1,IN_COLOR);
            draw_NODE(map,p,map->Line[abs(initial.arc)].N2,IN_COLOR);
           }
          else
           {
            if (initial.indicator=='4')
             {
/* "point" is the initial point as well as the ending */
/* point of a segment but NOT a node point of an arc */
/* Read initial arc */
              dig_P_read_line(map,(abs(initial.arc)),&p);
              if (initial.arc>0)
               {
/* Initial arc is going in a positive direction */
                for (i=(initial.segment+1); i < p->n_points; i++) 
                 {
/* Erase segment */
                  draw_SEGMENT(map,p,initial.arc,i,BG_COLOR);
/* Draw "red" segment */
                  draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                  dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                  last_m_pt.arc_dist_i_lm += dist;
                  terminal.arc_dist_i_t   += dist;
                 }
                if (DSLE_flag=='S'||DSLE_flag=='L')
                 {
                  fprintf(pipe_ptr,"PartArc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(initial.arc),initial.n,initial.e,p->y[(p->n_points-1)],p->x[(p->n_points-1)],terminal.arc_dist_i_t);
                  fprintf(pipe_ptr,"                                                                 %14.2lff\n",(terminal.arc_dist_i_t*3.2808));
                 }
               }
              else
               {
/* Initial arc is going in a negative direction */
                for (i=initial.segment; i > 0; i--)
                 {
/* Erase segment */
                  draw_SEGMENT(map,p,initial.arc,i,BG_COLOR);
/* Draw "red" segment */
                  draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                  dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                  last_m_pt.arc_dist_i_lm += dist;
                  terminal.arc_dist_i_t   += dist;
                 }
                if (DSLE_flag=='S'||DSLE_flag=='L')
                 {
                  fprintf(pipe_ptr,"PartArc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(initial.arc),initial.n,initial.e,p->y[0],p->x[0],terminal.arc_dist_i_t);
                  fprintf(pipe_ptr,"                                                                 %14.2lff\n",(terminal.arc_dist_i_t*3.2808));
                 }
               }
/* Redraw two end nodes for initial.arc */
              draw_NODE(map,p,map->Line[abs(initial.arc)].N1,IN_COLOR);
              draw_NODE(map,p,map->Line[abs(initial.arc)].N2,IN_COLOR);
             }
            else
             {
              if ((initial.indicator=='1')||(initial.indicator=='2'))
               {
                if ( ((initial.indicator=='1')&&(initial.arc>0)) ||
                     ((initial.indicator=='2')&&(initial.arc<0))   )
                 {
/* "point" is a beginning or ending node point of a line */
/* "point" is both initial point and node (N1) or node (N2) */
/* If "point" is node N1 then the arc must be positive */
/* If "point" is node N2 then the arc must be negative */
                  dig_P_read_line(map,(abs(initial.arc)),&p);
/* Erase arc */
                  draw_ARC(map,p,initial.arc,(int)0,BG_COLOR,BG_COLOR);
/* Draw "red" measured line */
                  draw_ARC(map,p,initial.arc,(int)0,MA_COLOR,MA_COLOR);
/* Calculate distance of "red" arc */
                  dist_ARC(map,p,initial.arc,DSLE_flag,&dist);
                  last_m_pt.arc_dist_i_lm += dist;
                  terminal.arc_dist_i_t   += dist;
                 }
/* Redraw two end nodes for initial.arc */
                draw_NODE(map,p,map->Line[abs(initial.arc)].N1,IN_COLOR);
                draw_NODE(map,p,map->Line[abs(initial.arc)].N2,IN_COLOR);
               }
             }
           }
         }
/* Calculate value for "last.dir_dist_i_lm" */
        distance(initial.e,initial.n,last_m_pt.e,last_m_pt.n,&dist);
        last_m_pt.dir_dist_i_lm = dist;
/* Assign "dist" value to "terminal.dir_dist_i_t" */
        terminal.dir_dist_i_t = dist;
       }
      else
       {
        if (ptr_arc_var == arc_t_info.addr_last_arc)
         {
#ifdef DEBUG_dmpe
fprintf(stderr,"dr_me_pr_ex:  Draw in \"red\" the terminal arc;  arc is %d\n",arc_num);
#endif DEBUG_dmpe

/* THIS IS THE TERMINAL ARC */

/* Redraw the terminal arc as the initial colors */
          draw_ARC(map,p,terminal.arc,(int)1,IA_COLOR1,IA_COLOR2);
/* Assign value from "last_m_pt.arc_dist_i_lm" to "terminal.arc_dist_i_t" */
/*  no longer needed...
          terminal.arc_dist_i_t = last_m_pt.arc_dist_i_lm;
*/
/* This portion of the code will draw the terminal arc */
          term_arc_dist = 0.0;
/* at last position in the "arc_table" structure */
/* draw portion of terminal arc */
/* terminal point */
          if (terminal.indicator=='0')
           {
/* "point" is a mid-point of a segment as well as the terminal point */
/* "point" is NOT an end point of a segment and is NOT a node */
/* Read terminal arc */
            dig_P_read_line(map,abs(terminal.arc),&p);
            if (terminal.arc > 0)
             {
/* "terminal.arc" is going in a positive direction */
/* Draw "red" line on this partial arc */
              for (i=1; i < terminal.segment; i++)
               {
/* Erase segment */
                draw_SEGMENT(map,p,terminal.arc,i,BG_COLOR);
/* Draw "red" segment */
                draw_SEGMENT(map,p,terminal.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                dist_SEGMENT(map,p,terminal.arc,i,DSLE_flag,&dist);
                terminal.arc_dist_i_t += dist;
                term_arc_dist += dist;
               }
/* Erase partial segment */
              draw_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,BG_COLOR);
/* Draw "red" partial segment */
              draw_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
              dist_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,terminal.arc,terminal.segment,DSLE_flag,&dist);
              terminal.arc_dist_i_t += dist;
              term_arc_dist += dist;
              if (DSLE_flag=='S'||DSLE_flag=='L')
               {
                fprintf(pipe_ptr,"PartArc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(terminal.arc),p->y[0],p->x[0],terminal.n,terminal.e,term_arc_dist);
                  fprintf(pipe_ptr,"                                                                 %14.2lff\n",(term_arc_dist*3.2808));
               }
             }
            else
             {
/* "terminal.arc" is going in a negative direction */
              for (i=(p->n_points-1); i > terminal.segment; i--)
               {
/* Erase segment */
                draw_SEGMENT(map,p,terminal.arc,i,BG_COLOR);
/* Draw "red" segment */
                draw_SEGMENT(map,p,terminal.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                dist_SEGMENT(map,p,terminal.arc,i,DSLE_flag,&dist);
                terminal.arc_dist_i_t += dist;
                term_arc_dist += dist;
               }
/* Erase partial segment */
              draw_seg(map,p,p->y[terminal.segment],p->x[terminal.segment],terminal.n,terminal.e,BG_COLOR);
/* Draw partial segment */
              draw_seg(map,p,p->y[terminal.segment],p->x[terminal.segment],terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
              dist_seg(map,p,p->y[terminal.segment],p->x[terminal.segment],terminal.n,terminal.e,terminal.arc,terminal.segment,DSLE_flag,&dist);
              terminal.arc_dist_i_t += dist;
              term_arc_dist += dist;
              if (DSLE_flag=='S'||DSLE_flag=='L')
               {
                fprintf(pipe_ptr,"PartArc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(terminal.arc),p->y[(p->n_points-1)],p->x[(p->n_points-1)],terminal.n,terminal.e,term_arc_dist);
                fprintf(pipe_ptr,"                                                                 %14.2lff\n",(term_arc_dist*3.2808));
               }
             }
/* Redraw two end nodes for terminal.arc */
            draw_NODE(map,p,map->Line[abs(terminal.arc)].N1,IN_COLOR);
            draw_NODE(map,p,map->Line[abs(terminal.arc)].N2,IN_COLOR);
           }
          else
           {
            if (terminal.indicator=='8')
             {
/* "point" is the terminal point as well as the beginning */
/* point of a segment but NOT a node point of an arc */
/* Read terminal arc */
              dig_P_read_line(map,(abs(terminal.arc)),&p);
              if (terminal.arc>0)
               {
/* "terminal.arc" is going in a positive direction */
                for (i=1; i < terminal.segment; i++) 
                 {
/* Erase segment */
                  draw_SEGMENT(map,p,terminal.arc,i,BG_COLOR);
/* Draw "red" segment */
                  draw_SEGMENT(map,p,terminal.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                  dist_SEGMENT(map,p,terminal.arc,i,DSLE_flag,&dist);
                  terminal.arc_dist_i_t += dist;
                  term_arc_dist += dist;
                 }
                if (DSLE_flag=='S'||DSLE_flag=='L')
                 {
                  fprintf(pipe_ptr,"PartArc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(terminal.arc),p->y[0],p->x[0],terminal.n,terminal.e,term_arc_dist);
                  fprintf(pipe_ptr,"                                                                 %14.2lff\n",(term_arc_dist*3.2808));
                 }
               }
              else
               {
/* "terminal.arc" is going in a negative direction */
                for (i=(p->n_points-1); i >= terminal.segment; i--)
                 {
/* Erase segment */
                  draw_SEGMENT(map,p,terminal.arc,i,BG_COLOR);
/* Draw "red" segment */
                  draw_SEGMENT(map,p,terminal.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                  dist_SEGMENT(map,p,terminal.arc,i,DSLE_flag,&dist);
                  terminal.arc_dist_i_t += dist;
                  term_arc_dist += dist;
                 }
                if (DSLE_flag=='S'||DSLE_flag=='L')
                 {
                  fprintf(pipe_ptr,"PartArc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(terminal.arc),p->y[(p->n_points-1)],p->x[(p->n_points-1)],terminal.n,terminal.e,term_arc_dist);
                  fprintf(pipe_ptr,"                                                                 %14.2lff\n",(term_arc_dist*3.2808));
                 }
               }
/* Redraw two end nodes for terminal.arc */
              draw_NODE(map,p,map->Line[abs(terminal.arc)].N1,IN_COLOR);
              draw_NODE(map,p,map->Line[abs(terminal.arc)].N2,IN_COLOR);
             }
            else
             {
              if (terminal.indicator=='9')
               {
/* "point" is the terminal point as well as the ending */
/* point of a segment but NOT a node point of an arc */
/* Read terminal arc */
                dig_P_read_line(map,(abs(terminal.arc)),&p);
                if (terminal.arc>0)
                 {
/* "terminal.arc" is going in a positive direction */
                  for (i=1; i <= terminal.segment; i++) 
                   {
/* Erase segment */
                    draw_SEGMENT(map,p,terminal.arc,i,BG_COLOR);
/* Draw "red" segment */
                    draw_SEGMENT(map,p,terminal.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                    dist_SEGMENT(map,p,terminal.arc,i,DSLE_flag,&dist);
                    terminal.arc_dist_i_t += dist;
                    term_arc_dist += dist;
                   }
                  if (DSLE_flag=='S'||DSLE_flag=='L')
                   {
                    fprintf(pipe_ptr,"PartArc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(terminal.arc),p->y[0],p->x[0],terminal.n,terminal.e,term_arc_dist);
                    fprintf(pipe_ptr,"                                                                 %14.2lff\n",(term_arc_dist*3.2808));
                   }
                 }
                else
                 {
/* "terminal.arc" is going in a negative direction */
                  for (i=(p->n_points-1); i > terminal.segment; i--)
                   {
/* Erase segment */
                    draw_SEGMENT(map,p,terminal.arc,i,BG_COLOR);
/* Draw "red" segment */
                    draw_SEGMENT(map,p,terminal.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                    dist_SEGMENT(map,p,terminal.arc,i,DSLE_flag,&dist);
                    terminal.arc_dist_i_t += dist;
                    term_arc_dist += dist;
                   }
                  if (DSLE_flag=='S'||DSLE_flag=='L')
                   {
                    fprintf(pipe_ptr,"PartArc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(terminal.arc),p->y[(p->n_points-1)],p->x[(p->n_points-1)],terminal.n,terminal.e,term_arc_dist);
                    fprintf(pipe_ptr,"                                                                 %14.2lff\n",(term_arc_dist*3.2808));
                   }
                 }
/* Redraw two end nodes for terminal.arc */
                draw_NODE(map,p,map->Line[abs(terminal.arc)].N1,IN_COLOR);
                draw_NODE(map,p,map->Line[abs(terminal.arc)].N2,IN_COLOR);
               }
              else
               {
                if (terminal.indicator=='6')
                 {
                  if (terminal.arc<0)
                   {
/* "point" is a beginning node point of a line */
/* "point" is both terminal point and node N1 */
                    dig_P_read_line(map,(abs(terminal.arc)),&p);
/* Erase arc */
                    draw_ARC(map,p,terminal.arc,(int)0,BG_COLOR,BG_COLOR);
/* Draw "red" measured line */
                    draw_ARC(map,p,terminal.arc,(int)0,MA_COLOR,MA_COLOR);
/* Calculate distance of "red" arc */
                    dist_ARC(map,p,terminal.arc,DSLE_flag,&dist);
                    terminal.arc_dist_i_t += dist;
/* Redraw two end nodes for terminal.arc */
                    draw_NODE(map,p,map->Line[abs(terminal.arc)].N1,IN_COLOR);
                    draw_NODE(map,p,map->Line[abs(terminal.arc)].N2,IN_COLOR);
                   }
                  else
                   {
#ifdef DEBUG_dmpe
fprintf(stderr,"dr_me_pr_ex:  terminal arc will NOT be drawn!!\n");
#endif DEBUG_dmpe
                   }
                 }
                else
                 {
                  if (terminal.indicator=='7')
                   {
                    if (terminal.arc>0)
                     {
#ifdef DEBUG_dmpe
fprintf(stderr,"dr_me_pr_ex:  WILL DRAW terminal.arc in its entirety\n"); 
#endif DEBUG_dmpe
/* "point" is a ending node point of a line */
/* "point" is both terminal point and node N2 */
                      dig_P_read_line(map,(abs(terminal.arc)),&p);
/* Erase arc */
                      draw_ARC(map,p,terminal.arc,(int)0,BG_COLOR,BG_COLOR);
/* Draw "red" measured line */
                      draw_ARC(map,p,terminal.arc,(int)0,MA_COLOR,MA_COLOR);
/* Calculate distance of "red" arc */
                      dist_ARC(map,p,terminal.arc,DSLE_flag,&dist);
                      terminal.arc_dist_i_t += dist;
/* Redraw two end nodes for terminal.arc */
                      draw_NODE(map,p,map->Line[abs(terminal.arc)].N1,IN_COLOR);
                      draw_NODE(map,p,map->Line[abs(terminal.arc)].N2,IN_COLOR);
                     }
                    else
                     {
#ifdef DEBUG_dmpe
fprintf(stderr,"dr_me_pr_ex:  terminal arc will NOT be drawn!!\n");
#endif DEBUG_dmpe
                     }
                   }
                 }
               }
             }
           }
          if (arc_t_info.count>2)
           {
/* This part will redraw all the arcs in its entirety (not including         */
/* the initial or terminal arcs).  This is necessary since your terminal     */
/* arc may be the same as one of these previously drawn red arcs.  If they   */
/* are then this terminal arc may have part of the arc in the IA_COLOR's and */
/* not in MA_COLOR.  This would have part of a previously drawn entire "red" */
/* arc in IA_COLOR's and not red.  This loop will redraw that previously     */
/* drawn "red" arc in their entirety if the terminal arc is the same as one  */ 
/* of those previously drawn arcs.  NOTE: that these arcs will be redrawn    */
/* but will NOT be remeasured since they have already been measured once.    */
            ptr_a_begin2 = ptr_arc + 1;
            ptr_a_end2   = ptr_arc + (arc_t_info.count-2);
            for (ptr_arc_var2=ptr_a_begin2; ptr_arc_var2 <= ptr_a_end2; ptr_arc_var2++)
             {
              arc_num2 = ptr_arc_var2->arc;
              if (abs(terminal.arc)==abs(arc_num2))
               {
/* Erase arc */
                draw_ARC(map,p,arc_num2,(int)0,BG_COLOR,BG_COLOR);
/* Draw "red" measured line */
                draw_ARC(map,p,arc_num2,(int)0,MA_COLOR,MA_COLOR);
/* Redraw two end nodes for initial.arc */
                draw_NODE(map,p,map->Line[abs(arc_num2)].N1,IN_COLOR);
                draw_NODE(map,p,map->Line[abs(arc_num2)].N2,IN_COLOR);
               }
             }
           }
/* If the abs(initial.arc) equals abs(terminal.arc) then redraw the initial */
/* arc in red but do NOT measure initial arc again. */
          if (abs(initial.arc)==abs(terminal.arc))
           {
            redraw_init(map,p,DSLE_flag);
           }
         }
        else
         {
#ifdef DEBUG_dmpe
fprintf(stderr,"dr_me_pr_ex:  Draw in \"red\" the arc in its entirety;  arc is NOT an initial or terminal arc; arc is %d\n",arc_num);
#endif DEBUG_dmpe
/* "point" is neither an initial or terminal point */
/* "point" is a beginning or ending node point of a line */
/* "point" is both initial point and node (N1) or node (N2) */
/* The indicator will be 'a' or 'b' */
          dig_P_read_line(map,(abs(arc_num)),&p);
/* Erase arc */
          draw_ARC(map,p,arc_num,(int)0,BG_COLOR,BG_COLOR);
/* Draw "red" arc */
          draw_ARC(map,p,arc_num,(int)0,MA_COLOR,MA_COLOR);
/* Calculate distance of "red" arc */
          dist_ARC(map,p,arc_num,DSLE_flag,&dist);
          last_m_pt.arc_dist_i_lm += dist;
          terminal.arc_dist_i_t   += dist;
/* Redraw two end nodes for initial.arc */
          draw_NODE(map,p,map->Line[abs(arc_num)].N1,IN_COLOR);
          draw_NODE(map,p,map->Line[abs(arc_num)].N2,IN_COLOR);
/* Calculate value for "last.dir_dist_i_lm" */
          distance(initial.e,initial.n,last_m_pt.e,last_m_pt.n,&dist);
          last_m_pt.dir_dist_i_lm = dist;
/* Assign "dist" value to "terminal.dir_dist_i_t" */
          terminal.dir_dist_i_t = dist;
#ifdef DEBUG_dmpe
fprintf(stderr,"\ndr_me_pr_ex:  have drawn the RED LINE in its entirety for arc: %d\n",arc_num);
#endif DEBUG_dmpe
         }    
       }
     }
   }
/* will print total arc distance and direct distance here, later... */
/* Calculate value for "terminal.dir_dist_i_t" */
  distance(initial.e,initial.n,terminal.e,terminal.n,&dist);
  terminal.dir_dist_i_t = dist;
  return(1);
 }
