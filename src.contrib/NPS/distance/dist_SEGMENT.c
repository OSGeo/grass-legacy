#include "distance.h"
int
dist_SEGMENT(map,p,arc,segment,DSLE_flag,dist)
  struct Map_info *map;
  struct line_pnts *p;
  int arc;
  int segment;
  char DSLE_flag;
  double *dist;
 {
  double x1,y1,x2,y2;

#ifdef DEBUG_dS
fprintf(stderr,"dist_SEGMENT:  arc=%d segment=%d\n",arc,segment);
#endif DEBUG_dS
  *dist = 0.0;
  dig_P_read_line(map,abs(arc),&p);
  if ((segment>(p->n_points-1))||(segment<1))
    return(0);
  if (arc>0)
   {
    x1 = p->x[segment-1];
    y1 = p->y[segment-1];
    x2 = p->x[segment];
    y2 = p->y[segment];
   }
  else
   {
    x1 = p->x[segment];
    y1 = p->y[segment];
    x2 = p->x[segment-1];
    y2 = p->y[segment-1];
   }
  distance(x1,y1,x2,y2,dist);
  if (DSLE_flag=='D')
   {
    total_points += 1;
   }
  else
   {
    if (DSLE_flag=='L')
     {
/* Write to "long print out" file the partial segment */
      fprintf(pipe_ptr,"A%4.4d:S%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(arc),segment,y1,x1,y2,x2,*dist);
      fprintf(pipe_ptr,"                                                                 %14.2lff\n",((*dist)*3.2808));
     }
    else
     {
      if (DSLE_flag=='E')
       {
#ifdef ASCII
/* "ASCII"                                                                   */
/* Write to "extract" file (dig_asc), the first point of the segment.        */
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
  return(1);
 }
