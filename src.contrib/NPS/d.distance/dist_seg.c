#include "distance.h"
int
dist_seg(map,p,n1,e1,n2,e2,arc,segment,DSLE_flag,dist)
  struct Map_info *map;
  struct line_pnts *p;
  double n1,e1,n2,e2;
  int arc, segment;
  char DSLE_flag;
  double *dist;
 {

  *dist = 0.0;
  if (n1==n2&&e1==e2)
   {
    *dist = 0.0;
   }
  else
   {
    distance(e1,n1,e2,n2,dist);
   }
  if (DSLE_flag=='D')
   {
    total_points += 1;
   }
  else
   {
    if (DSLE_flag=='L')
     {
/* Write to "long print out" file the partial segment */
      fprintf(pipe_ptr,"A%4.4d:P%4.4d:N%12.2lf E%11.2lf-N%12.2lf E%11.2lf:%13.2lfm\n",abs(arc),segment,n1,e1,n2,e2,*dist);
      fprintf(pipe_ptr,"                                                                 %14.2lff\n",((*dist)*3.2808));
     }
    else
     {
      if (DSLE_flag=='E')
       {
#ifdef ASCII
/* "ASCII"                                                                   */
/* Write to "extract" file (dig_asc), the first point of the segment.        */
        fprintf(dig_asc," %12.2lf %12.2lf\n",n1,e1);
#else
/* "BINARY"                                                                  */
/* Assign point of measured line to "x_array" and "y_array".                 */
        *(x_array+x_counter) = e1;
        x_counter += 1;
        *(y_array+y_counter) = n1;
        y_counter += 1;
#endif ASCII
       }
     }
   }
#ifdef DEBUG_ds
fprintf(stderr,"dist_seg:  total_points=%d\n",total_points);
#endif DEBUG_ds
  return(1);
 }
