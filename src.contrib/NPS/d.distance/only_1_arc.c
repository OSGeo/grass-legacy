
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
only_1_arc(map,p,DSLE_flag)
 struct Map_info *map;
 struct line_pnts *p;
 char DSLE_flag;
 {
  int i;
  double dist;

#ifdef DEBUG
fprintf(stderr,"only_1_arc\n");
#endif DEBUG
#ifdef DEBUG_o1a
fprintf(stderr,"\nonly_1_arc:  initial.arc=%d\n",initial.arc);
fprintf(stderr,"only_1_arc:  initial.segment=%d\n",initial.segment);
fprintf(stderr,"only_1_arc:  initial.indicator=%c\n",initial.indicator);
fprintf(stderr,"only_1_arc:  terminal.arc=%d\n",terminal.arc);
fprintf(stderr,"only_1_arc:  terminal.segment=%d\n",terminal.segment);
fprintf(stderr,"only_1_arc:  terminal.indicator=%c\n\n",terminal.indicator);
#endif DEBUG_o1a

  dig_P_read_line(map,abs(initial.arc),&p);
  draw_ARC(map,p,initial.arc,(int)1,IA_COLOR1,IA_COLOR2);
  if (initial.indicator=='1')
   {
/* "initial point" is at node N1 for the initial.arc */
    if (terminal.indicator=='6')
     {
/* "terminal point" is at node N1 for the terminal.arc */
/* Initial point and terminal point are one of the same. */
      dist_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,terminal.arc,terminal.segment,DSLE_flag,&dist);
      terminal.arc_dist_i_t = 0.0;
      terminal.dir_dist_i_t = 0.0;
     }
    else
     {
      if (terminal.indicator=='7')
       {
/* "terminal point" is at node N2 for the terminal.arc */
/* Draw "red" arc */
        draw_ARC(map,p,initial.arc,(int)0,MA_COLOR,MA_COLOR);
/* Calculate distance of "red" arc */
        dist_ARC(map,p,initial.arc,DSLE_flag,&dist);
        terminal.arc_dist_i_t += dist;
       }
      else
       {
        if (terminal.indicator=='8')
         {
/* "terminal point" is at the beginning point of terminal.segment and */
/* is NOT a node point */
          if (initial.arc>0)
           {
            for (i=1; i < terminal.segment; i++)
             {
/* Draw "red" segment */
              draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
              dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
              terminal.arc_dist_i_t += dist;
             }
           }
         }
        else
         {
          if (terminal.indicator=='9')
           {
/* "terminal point" is at the ending point of terminal.segment and */
/* is NOT a node point */
            if (initial.arc>0)
             {
              for (i=1; i <= terminal.segment; i++)
               {
/* Draw "red" segment */
                draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                terminal.arc_dist_i_t += dist;
               }
             }
           }
          else
           {
            if (terminal.indicator=='0')
             {
/* "terminal point" is a middle point of the terminal.segment and */
/* is NOT the beginining or ending point of terminal.segmnt and */
/* is NOT a node point */
              if (initial.arc>0)
               {
                for (i=1; i < terminal.segment; i++)
                 {
/* Draw "red" segment */
                  draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                  dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                  terminal.arc_dist_i_t += dist;
                 }
/* Draw "red" partial segment */
                draw_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                dist_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,terminal.arc,terminal.segment,DSLE_flag,&dist);
                terminal.arc_dist_i_t += dist;
               }
             }
           }
         }
       }
     }
   }
  else
   {
    if (initial.indicator=='2')
     {
/* "initial point" is at node N2 for the initial.arc */
      if (terminal.indicator=='6')
       {
/* "terminal point" is at node N1 for the terminal.arc */
        if (initial.arc<0)
         {
/* Draw "red" arc */
          draw_ARC(map,p,initial.arc,(int)0,MA_COLOR,MA_COLOR);
/* Calculate distance of "red" arc */
          dist_ARC(map,p,initial.arc,DSLE_flag,&dist);
          terminal.arc_dist_i_t += dist;
         }
       }
      else
       {
        if (terminal.indicator=='7')
         {
/* "terminal point" is at node N2 for the terminal.arc */
/* Initial point and terminal point are one of the same. */
          dist_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,terminal.arc,terminal.segment,DSLE_flag,&dist);
          terminal.arc_dist_i_t = 0.0;
          terminal.dir_dist_i_t = 0.0;
         }
        else
         {
          if (terminal.indicator=='8')
           {
/* "terminal point" is at the beginning point of terminal.segment and */
/* is NOT a node point */
            if (initial.arc<0)
             {
              for (i=(p->n_points-1); i >= terminal.segment; i--)
               {
/* Draw "red" segment */
                draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                terminal.arc_dist_i_t += dist;
               }
             }
           }
          else
           {
            if (terminal.indicator=='9')
             {
/* "terminal point" is at the ending point of terminal.segment and */
/* is NOT a node point */
              if (initial.arc<0)
               {
                for (i=(p->n_points-1); i > terminal.segment; i--)
                 {
/* Draw "red" segment */
                  draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                  dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                  terminal.arc_dist_i_t += dist;
                 }
               }
             }
            else
             {
              if (terminal.indicator=='0')
               {
/* "terminal point" is a middle point of the terminal.segment and */
/* is NOT the beginining or ending point of terminal.segmnt and */
/* is NOT a node point */
                if (initial.arc<0)
                 {
                  for (i=(p->n_points-1); i > terminal.segment; i--)
                   {
/* Draw "red" segment */
                    draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                    dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                    terminal.arc_dist_i_t += dist;
                   }
/* Draw "red" partial segment */
                  draw_seg(map,p,p->y[terminal.segment],p->x[terminal.segment],terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                  dist_seg(map,p,p->y[terminal.segment],p->x[terminal.segment],terminal.n,terminal.e,terminal.arc,terminal.segment,DSLE_flag,&dist);
                  terminal.arc_dist_i_t += dist;
                 }
               }
             }
           }
         }
       }
     }
    else
     {
      if (initial.indicator=='3')
       {
/* "initial point" is at the beginning point of initial.segment and */
/* is NOT a node point */
        if (terminal.indicator=='6')
         {
/* "terminal point" is at node N1 for the terminal.arc */
          if (initial.arc<0)
           {
            for (i=(initial.segment-1); i > 0; i--) 
             {
/* Draw "red" segment */
              draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
              dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
              terminal.arc_dist_i_t += dist;
             }
           }
         }
        else
         {
          if (terminal.indicator=='7')
           {
/* "terminal point" is at node N2 for the terminal.arc */
            if (initial.arc>0)
             {
              for (i=initial.segment; i < p->n_points; i++)
               {
/* Draw "red" segment */
                draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                terminal.arc_dist_i_t += dist;
               }
             }
           }
          else
           {
            if (terminal.indicator=='8')
             {
/* "terminal point" is at the beginning point of terminal.segment and */
/* is NOT a node point */
              if (initial.arc>0)
               {
/* initial.arc direction is positive */
                if (initial.segment==terminal.segment)
                 {
                  terminal.arc_dist_i_t = 0.0;
                  terminal.dir_dist_i_t = 0.0;
                 }
                else
                 {
                  for (i=initial.segment; i < terminal.segment; i++)
                   {
/* Draw "red" segment */
                    draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                    dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                    terminal.arc_dist_i_t += dist;
                   }
                 }
               }
              else
               {
/* initial.arc direction is negative */
                if (initial.segment==terminal.segment)
                 {
                  terminal.arc_dist_i_t = 0.0;
                  terminal.dir_dist_i_t = 0.0;
                 }
                else
                 {
                  for (i=(initial.segment-1); i >= terminal.segment; i--)
                   {
/* Draw "red" segment */
                    draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                    dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                    terminal.arc_dist_i_t += dist;
                   }
                 }
               }
             }
            else
             {
              if (terminal.indicator=='9')
               {
/* "terminal point" is at the ending point of terminal.segment and */
/* is NOT a node point */
                if (initial.arc>0) 
                 {
/* initial.arc direction is positive */
                  if ( initial.segment==(terminal.segment-1) )
                   {
                    terminal.arc_dist_i_t = 0.0;
                    terminal.dir_dist_i_t = 0.0;
                   }
                  else
                   {
                    for (i=initial.segment; i <= terminal.segment; i++)
                     {
/* Draw "red" segment */
                      draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                      dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                      terminal.arc_dist_i_t += dist;
                     }
                   }
                 }
                else
                 {
/* initial.arc direction is negative */
                  if ( initial.segment==(terminal.segment-1) )
                   {
                    terminal.arc_dist_i_t = 0.0;
                    terminal.dir_dist_i_t = 0.0;
                   }
                  else
                   {
                    for (i=(initial.segment-1); i > terminal.segment; i--)
                     {
/* Draw "red" segment */
                      draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                      dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                      terminal.arc_dist_i_t += dist;
                     }
                   }
                 }
               }
              else
               {
                if (terminal.indicator=='0')
                 {
/* "terminal point" is a middle point of the terminal.segment and */
/* is NOT the beginining or ending point of terminal.segmnt and */
/* is NOT a node point */
                  if (initial.arc>0)
                   {
/* initial.arc direction is positive */
                    if (initial.segment==terminal.segment)
                     {
/* Draw "red" segment */
                      draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                      dist_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,initial.arc,initial.segment,DSLE_flag,&dist);
                      terminal.arc_dist_i_t += dist;
                     }
                    else
                     {
                      for (i=initial.segment; i < terminal.segment; i++) 
                       {
/* Draw "red" segment */
                        draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                        dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                        terminal.arc_dist_i_t += dist;
                       }
/* Draw "red" partial segment */
                      draw_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                      dist_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,terminal.arc,terminal.segment,DSLE_flag,&dist);
                      terminal.arc_dist_i_t += dist;
                     }
                   }
                  else
                   {
/* initial.arc direction is negative */
                    if ( (initial.segment-1)==terminal.segment )
                     {
/* Draw "red" partial segment */
                      draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                      dist_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,initial.arc,initial.segment,DSLE_flag,&dist);
                      terminal.arc_dist_i_t += dist;
                     }
                    else
                     {
                      for (i=(initial.segment-1); i > terminal.segment; i--) 
                       {
/* Draw "red" segment */
                        draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                        dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                        terminal.arc_dist_i_t += dist;
                       }
/* Draw "red" partial segment */
                      draw_seg(map,p,p->y[terminal.segment],p->x[terminal.segment],terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                      dist_seg(map,p,p->y[terminal.segment],p->x[terminal.segment],terminal.n,terminal.e,terminal.arc,terminal.segment,DSLE_flag,&dist);
                      terminal.arc_dist_i_t += dist;
                     }
                   }
                 }
               }
             }
           }
         }
       }
      else
       {
        if (initial.indicator=='4')
         {
/* "initial point" is at the ending point of initial.segment and */
/* is NOT a node point */
          if (terminal.indicator=='6')
           {
/* "terminal point" is at node N1 for the terminal.arc */
            if (initial.arc<0)
             {
              for (i=initial.segment; i > 0; i--)  
               {
/* Draw "red" segment */
                draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                terminal.arc_dist_i_t += dist;
               }
             }
           }
          else
           {
            if (terminal.indicator=='7')
             {
/* "terminal point" is at node N2 for the terminal.arc */
              if (initial.arc>0)
               {
                for (i=(initial.segment+1); i < p->n_points; i++)
                 {
/* Draw "red" segment */
                  draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                  dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                  terminal.arc_dist_i_t += dist;
                 }
               }
             }
            else
             {
              if (terminal.indicator=='8')
               {
/* "terminal point" is at the beginning point of terminal.segment and */
/* is NOT a node point */
                if (initial.arc>0)
                 {
/* initial.arc direction is positive */
                  if (initial.segment==(terminal.segment-1))
                   {
                    terminal.arc_dist_i_t = 0.0;
                    terminal.dir_dist_i_t = 0.0;
                   }
                  else
                   {
                    for (i=(initial.segment+1); i < terminal.segment; i++)
                     {
/* Draw "red" segment */
                      draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                      dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                      terminal.arc_dist_i_t += dist;
                     }
                   }
                 }
                else
                 {
/* initial.arc direction is negative */
                  if (initial.segment==(terminal.segment-1))
                   {
                    terminal.arc_dist_i_t = 0.0;
                    terminal.dir_dist_i_t = 0.0;
                   }
                  else
                   {
                    for (i=initial.segment; i >= terminal.segment; i--)
                     {
/* Draw "red" segment */
                      draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                      dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                      terminal.arc_dist_i_t += dist;
                     }
                   }
                 }
               }
              else
               {
                if (terminal.indicator=='9')
                 {
/* "terminal point" is at the ending point of terminal.segment and */
/* is NOT a node point */
                  if (initial.arc>0)
                   { 
/* initial.arc direction is positive */
                    if (initial.segment==terminal.segment)
                     {
                      terminal.arc_dist_i_t = 0.0;
                      terminal.dir_dist_i_t = 0.0;
                     }
                    else
                     {
                      for (i=(initial.segment+1); i <= terminal.segment; i++)
                       {
/* Draw "red" segment */
                        draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                        dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                        terminal.arc_dist_i_t += dist;
                       }
                     }
                   }
                  else
                   {
/* initial.arc direction is negative */
                    if (initial.segment==terminal.segment)
                     {
                      terminal.arc_dist_i_t = 0.0;
                      terminal.dir_dist_i_t = 0.0;
                     }
                    else
                     {
                      for (i=initial.segment; i > terminal.segment; i--)
                       {
/* Draw "red" segment */
                        draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                        dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                        terminal.arc_dist_i_t += dist;
                       }
                     }
                   }
                 }
                else
                 {
                  if (terminal.indicator=='0') 
                   {
/* "terminal point" is a middle point of the terminal.segment and */
/* is NOT the beginining or ending point of terminal.segmnt and */
/* is NOT a node point */
                    if (initial.arc>0)
                     {
/* initial.arc direction is positive */
                      if ((initial.segment+1)==terminal.segment)
                       {
/* Draw "red" partial segment */
                        draw_seg(map,p,p->y[initial.segment],p->x[initial.segment],terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                        dist_seg(map,p,p->y[initial.segment],p->x[initial.segment],terminal.n,terminal.e,initial.arc,initial.segment,DSLE_flag,&dist);
                        terminal.arc_dist_i_t += dist;
                       }
                      else
                       {
                        for (i=(initial.segment+1); i < terminal.segment; i++)
                         {
/* Draw "red" segment */
                          draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                          dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                          terminal.arc_dist_i_t += dist;
                         }
/* Draw "red" partial segment */
                        draw_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                        dist_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,terminal.arc,terminal.segment,DSLE_flag,&dist);
                        terminal.arc_dist_i_t += dist;
                       }
                     }
                    else
                     {
/* initial.arc direction is negative */
                      if (initial.segment==terminal.segment)
                       {
/* Draw "red" partial segment */
                        draw_seg(map,p,p->y[initial.segment],p->x[initial.segment],terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                        dist_seg(map,p,p->y[initial.segment],p->x[initial.segment],terminal.n,terminal.e,initial.arc,initial.segment,DSLE_flag,&dist);
                        terminal.arc_dist_i_t += dist;
                       }
                      else
                       {
                        for (i=initial.segment; i > terminal.segment; i--)
                         {
/* Draw "red" segment */
                          draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                          dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                          terminal.arc_dist_i_t += dist;
                         }
/* Draw "red" partial segment */
                        draw_seg(map,p,p->y[terminal.segment],p->x[terminal.segment],terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                        dist_seg(map,p,p->y[terminal.segment],p->x[terminal.segment],terminal.n,terminal.e,terminal.arc,terminal.segment,DSLE_flag,&dist);
                        terminal.arc_dist_i_t += dist;
                       }
                     }
                   }
                 }
               }
             }
           }
         }
        else
         {
          if (initial.indicator=='5')
           {
/* "initial point" is a middle point of the initial.segment and */
/* is NOT the beginining or ending point of initial.segmnt and */
/* is NOT a node point */
            if (terminal.indicator=='6')
             {
/* "terminal point" is at node N1 for the terminal.arc */
              if (initial.arc<0)
               {
/* initial.arc direction is negative */
                if (initial.segment==terminal.segment)
                 {
/* Draw "red" partial segment */
                  draw_seg(map,p,initial.n,initial.e,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],MA_COLOR);
/* Calculate distance of "red" partial segment */
                  dist_seg(map,p,initial.n,initial.e,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.arc,terminal.segment,DSLE_flag,&dist);
                  terminal.arc_dist_i_t += dist;
                 }
                else
                 {
/* initial.arc direction is negative */
/* Draw "red" partial segment */
                  draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],MA_COLOR);
/* Calculate distance of "red" partial segment */
                  dist_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],initial.arc,initial.segment,DSLE_flag,&dist);
                  terminal.arc_dist_i_t += dist;
                  for (i=(initial.segment-1); i > 0; i--)
                   {
/* Draw "red" segment */
                    draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                    dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                    terminal.arc_dist_i_t += dist;
                   }
                 }
               }
             }
            else
             {
              if (terminal.indicator=='7')
               {
/* "terminal point" is at node N2 for the terminal.arc */
                if (initial.arc>0)
                 {
/* initial.arc direction is positive */
                  if (initial.segment==terminal.segment)
                   {
/* Draw "red" partial segment */
                    draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                    dist_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,initial.arc,initial.segment,DSLE_flag,&dist);
                    terminal.arc_dist_i_t += dist;
                   }
                  else
                   {
/* Draw "red" partial segment */
                    draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],MA_COLOR);
/* Calculate distance of "red" partial segment */
                    dist_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],initial.arc,initial.segment,DSLE_flag,&dist);
                    terminal.arc_dist_i_t += dist;
                    for (i=(initial.segment+1); i < p->n_points; i++)
                     {
/* Draw "red" segment */
                      draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                      dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                      terminal.arc_dist_i_t += dist;
                     }
                   }
                 }
               }
              else
               {
                if (terminal.indicator=='8')
                 {
/* "terminal point" is at the beginning point of terminal.segment and */
/* is NOT a node point */
                  if (initial.arc>0)
                   {
/* initial.arc direction is positive */
                    if (initial.segment==(terminal.segment-1))
                     {
/* Draw "red" partial segment */
                      draw_seg(map,p,initial.n,initial.e,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],MA_COLOR);
/* Calculate distance of "red" partial segment */
                      dist_seg(map,p,initial.n,initial.e,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.arc,terminal.segment,DSLE_flag,&dist);
                      terminal.arc_dist_i_t += dist;
                     }
                    else
                     {
/* Draw "red" partial segment */
                      draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],MA_COLOR);
/* Calculate distance of "red" partial segment */
                      dist_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],initial.arc,initial.segment,DSLE_flag,&dist);
                      terminal.arc_dist_i_t += dist;
                      for (i=(initial.segment+1); i < terminal.segment; i++)
                       {
/* Draw "red" segment */
                        draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                        dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                        terminal.arc_dist_i_t += dist;
                       }
                     }
                   }
                  else
                   {
/* initial.arc direction is negative */
                    if (initial.segment==terminal.segment)
                     {
/* Draw "red" partial segment */
                      draw_seg(map,p,initial.n,initial.e,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],MA_COLOR);
/* Calculate distance of "red" partial segment */
                      dist_seg(map,p,initial.n,initial.e,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.arc,terminal.segment,DSLE_flag,&dist);
                      terminal.arc_dist_i_t += dist;
                     }
                    else
                     {
/* Draw "red" partial segment */
                      draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],MA_COLOR);
/* Calculate distance of "red" partial segment */
                      dist_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],initial.arc,initial.segment,DSLE_flag,&dist);
                      terminal.arc_dist_i_t += dist;
                      for (i=(initial.segment-1); i >= terminal.segment; i--)
                       {
/* Draw "red" segment */
                        draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                        dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                        terminal.arc_dist_i_t += dist;
                       }
                     }
                   }
                 }
                else
                 {
                  if (terminal.indicator=='9')
                   {
/* "terminal point" is at the ending point of terminal.segment and */
/* is NOT a node point */
                    if (initial.arc>0)
                     {
/* initial.arc direction is positive */
                      if (initial.segment==terminal.segment)
                       {
/* Draw "red" partial segment */
                        draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                        dist_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,initial.arc,initial.segment,DSLE_flag,&dist);
                        terminal.arc_dist_i_t += dist;
                       }
                      else
                       {
/* Draw "red" partial segment */
                        draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],MA_COLOR);
/* Calculate distance of "red" partial segment */
                        dist_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],initial.arc,initial.segment,DSLE_flag,&dist);
                        terminal.arc_dist_i_t += dist;
                        for (i=(initial.segment+1); i <= terminal.segment; i++)
                         {
/* Draw "red" segment */
                          draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                          dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                          terminal.arc_dist_i_t += dist;
                         }
                       }
                     }
                    else
                     {
/* initial.arc direction is negative */
                      if ((initial.segment-1)==terminal.segment)
                       {
/* Draw "red" partial segment */
                        draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                        dist_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,initial.arc,initial.segment,DSLE_flag,&dist);
                        terminal.arc_dist_i_t += dist;
                       }
                      else
                       {
/* Draw "red" partial segment */
                        draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],MA_COLOR);
/* Calculate distance of "red" partial segment */
                        dist_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],initial.arc,initial.segment,DSLE_flag,&dist);
                        terminal.arc_dist_i_t += dist;
                        for (i=(initial.segment-1); i > terminal.segment; i--)
                         {
/* Draw "red" segment */
                          draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                          dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                          terminal.arc_dist_i_t += dist;
                         }
                       }
                     }
                   }
                  else
                   {
                    if (terminal.indicator=='0')
                     {
/* "terminal point" is a middle point of the terminal.segment and */
/* is NOT the beginining or ending point of terminal.segmnt and */
/* is NOT a node point */
                      if (initial.arc>0)
                       {
/* initial.arc direction is positive */
                        if (initial.segment==terminal.segment)
                         {
/* Erase partial segment */
                          draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,BG_COLOR);
/* Draw "red" partial segment */
                          draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                          dist_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,initial.arc,initial.segment,DSLE_flag,&dist);
                          terminal.arc_dist_i_t += dist;
                         }
                        else
                         {
/* Erase partial initial segment */
                          draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],BG_COLOR);
/* Draw "red" partial initial segment */
                          draw_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],MA_COLOR);
/* Calculate distance of "red" partial segment */
                          dist_seg(map,p,initial.n,initial.e,p->y[initial.segment],p->x[initial.segment],initial.arc,initial.segment,DSLE_flag,&dist);
                          terminal.arc_dist_i_t += dist;
                          for (i=(initial.segment+1); i < terminal.segment; i++)
                           {
/* Erase segment */
                            draw_SEGMENT(map,p,initial.arc,i,BG_COLOR);
/* Draw "red" segment */
                            draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                            dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                            terminal.arc_dist_i_t += dist;
                           }
/* Erase partial terminal segment */
                          draw_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,BG_COLOR);
/* Draw "red" partial terminal segment */
                          draw_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                          dist_seg(map,p,p->y[(terminal.segment-1)],p->x[(terminal.segment-1)],terminal.n,terminal.e,terminal.arc,terminal.segment,DSLE_flag,&dist);
                          terminal.arc_dist_i_t += dist;
                         }
                       }
                      else
                       {
/* initial.arc direction is negative */
                        if (initial.segment==terminal.segment)
                         {
/* Erase partial segment */
                          draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,BG_COLOR);
/* Draw "red" partial segment */
                          draw_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                          dist_seg(map,p,initial.n,initial.e,terminal.n,terminal.e,initial.arc,initial.segment,DSLE_flag,&dist);
                          terminal.arc_dist_i_t += dist;
                         }
                        else
                         {
/* Erase partial initial segment */
                          draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],BG_COLOR);
/* Draw "red" partial initial segment */
                          draw_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],MA_COLOR);
/* Calculate distance of "red" partial segment */
                          dist_seg(map,p,initial.n,initial.e,p->y[(initial.segment-1)],p->x[(initial.segment-1)],initial.arc,initial.segment,DSLE_flag,&dist);
                          terminal.arc_dist_i_t += dist;
                          for (i=(initial.segment-1); i > terminal.segment; i--)
                           {
/* Erase segment */
                            draw_SEGMENT(map,p,initial.arc,i,BG_COLOR);
/* Draw "red" segment */
                            draw_SEGMENT(map,p,initial.arc,i,MA_COLOR);
/* Calculate distance of "red" segment */
                            dist_SEGMENT(map,p,initial.arc,i,DSLE_flag,&dist);
                            terminal.arc_dist_i_t += dist;
                           }
/* Erase partial terminal segment */
                          draw_seg(map,p,p->y[terminal.segment],p->x[terminal.segment],terminal.n,terminal.e,BG_COLOR);
/* Draw "red" partial terminal segment */
                          draw_seg(map,p,p->y[terminal.segment],p->x[terminal.segment],terminal.n,terminal.e,MA_COLOR);
/* Calculate distance of "red" partial segment */
                          dist_seg(map,p,p->y[terminal.segment],p->x[terminal.segment],terminal.n,terminal.e,terminal.arc,terminal.segment,DSLE_flag,&dist);
                          terminal.arc_dist_i_t += dist;
                         }
                       }
                     }
                   }
                 }
               }
             }
           }
         }
       }
     }
   }
/* Redraw two end nodes for initial.arc */
  draw_NODE(map,p,map->Line[abs(initial.arc)].N1,IN_COLOR);
  draw_NODE(map,p,map->Line[abs(initial.arc)].N2,IN_COLOR);
  return(1);
 }
