#include "distance.h"
int
dist_ARC(map,p,arc,DSLE_flag,dist)
  struct Map_info *map;
  struct line_pnts *p;
  int arc;
  char DSLE_flag;
  double *dist;
 {
  double seg_dist;
  int i;
  double x1,y1,x2,y2;
 
  *dist = 0.0;
  if (arc_is_dot(map,p,arc)!=1)
   {
    dig_P_read_line(map,abs(arc),&p);
    if (arc>0)
     {
/* "arc" is positive                                                         */ 
      for (i=0; i < (p->n_points-1); i++)
       {
        x1 = p->x[i];
        y1 = p->y[i];
        x2 = p->x[i+1];
        y2 = p->y[i+1];
        distance(x1,y1,x2,y2,&seg_dist);
        *dist += seg_dist;
        if (DSLE_flag=='D')
         {
          total_points += 1;
         }
        else
         {
          if (DSLE_flag=='L')
           {
            fprintf(pipe_ptr,"A%4.4d:S%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(arc),(i+1),y1,x1,y2,x2,seg_dist);
            fprintf(pipe_ptr,"                                                                 %14.2lff\n",(seg_dist*3.2808));
           }
          else
           {
            if (DSLE_flag=='E')
             {
#ifdef ASCII
/* "ASCII"                                                                   */
/* Write to "extract" file (dig_asc), one point.                             */
              fprintf(dig_asc," %12.2lf %12.2lf\n",y1,x1);
#else
/* "BINARY"                                                                  */
/* Assign point of measured line to "x_array" and "y_array".                 */
              *(x_array+x_counter) = x1;
              x_counter += 1;
              *(y_array+y_counter) = y1;
              y_counter += 1;
#endif ASCII
             }
           }
         }
       }
      if (DSLE_flag=='S'||DSLE_flag=='L')
       {
        fprintf(pipe_ptr,"    Arc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(arc),p->y[0],p->x[0],p->y[(p->n_points-1)],p->x[(p->n_points-1)],*dist);
        fprintf(pipe_ptr,"                                                                 %14.2lff\n",((*dist)*3.2808));
       }
     }
    else
     {
/* "arc" is negative                                                         */ 
      for (i=(p->n_points-1); i > 0; i--)
       {
        x1 = p->x[i];
        y1 = p->y[i];
        x2 = p->x[i-1];
        y2 = p->y[i-1];
        distance(x1,y1,x2,y2,&seg_dist);
        *dist += seg_dist;
        if (DSLE_flag=='D')
         {
          total_points += 1;
         }
        else
         {
          if (DSLE_flag=='L')
           {
            fprintf(pipe_ptr,"A%4.4d:S%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(arc),i,y1,x1,y2,x2,seg_dist);
            fprintf(pipe_ptr,"                                                                 %14.2lff\n",(seg_dist*3.2808));
           }
          else
           {
            if (DSLE_flag=='E')
             {
#ifdef ASCII
/* "ASCII"                                                                   */
/* Write to "extract" file (dig_asc), one point.                             */
              fprintf(dig_asc," %12.2lf %12.2lf\n",y1,x1);
#else
/* "BINARY"                                                                  */
/* Assign point of measured line to "x_array" and "y_array".                 */
              *(x_array+x_counter) = x1;
              x_counter += 1;
              *(y_array+y_counter) = y1;
              y_counter += 1;
#endif ASCII
             }
           }
         }
       }
      if (DSLE_flag=='S'||DSLE_flag=='L')
       {
        fprintf(pipe_ptr,"    Arc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(arc),p->y[(p->n_points-1)],p->x[(p->n_points-1)],p->y[0],p->x[0],*dist);
        fprintf(pipe_ptr,"                                                                 %14.2lff\n",((*dist)*3.2808));
       }
     }
   }
  else
   {
/* Arc is a dot */
    if (DSLE_flag=='D')
     {
      total_points += 1;
     }
    else
     {
      if (DSLE_flag=='L')
       {
        fprintf(pipe_ptr,"    Arc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(arc),p->y[0],p->x[0],p->y[(p->n_points-1)],p->x[(p->n_points-1)],*dist);
        fprintf(pipe_ptr,"                                                                 %14.2lff\n",((*dist)*3.2808));
       }
      else
       {
        if (DSLE_flag=='S')
         {
          fprintf(pipe_ptr,"    Arc%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(arc),p->y[0],p->x[0],p->y[(p->n_points-1)],p->x[(p->n_points-1)],*dist);
          fprintf(pipe_ptr,"                                                                 %14.2lff\n",((*dist)*3.2808));
         }
        else
         {
          if (DSLE_flag=='E')
           {
#ifdef ASCII
/* "ASCII"                                                                   */
/* Write to "extract" file (dig_asc), one point.                             */
            fprintf(dig_asc," %12.2lf %12.2lf\n",p->y[0],p->x[0]);
#else
/* "BINARY"                                                                  */
/* Assign point of measured line to "x_array" and "y_array".                 */
            *(x_array+x_counter) = p->x[0]; 
            x_counter += 1;
            *(y_array+y_counter) = p->y[0];
            y_counter += 1;
#endif ASCII
           }
         }
       }
     }
   }
  return(1);
 }
