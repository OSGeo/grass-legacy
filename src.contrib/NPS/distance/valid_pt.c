/* Function "valid_pt" determines if point (pt_e,pt_n) can be a valid        */
/* terminal point on the terminal segment.                                   */
/* This function will only be called when abs(initial.arc) equals            */
/* abs(terminal.arc) and initial.segment equals terminal.segment and         */
/* also only if arc_t_info.count equals one.                                 */ 
#include "distance.h"
#include <math.h>
int valid_pt(map,p,pt_n,pt_e)
 struct Map_info *map;
 struct line_pnts *p;
 double pt_n,pt_e;
 {
  double b_x, b_y, e_x, e_y;
  double dist_b_i, dist_b_t, dist_b_pt;
  double dist_e_i, dist_e_t, dist_e_pt;
  double dist;

#ifdef DEBUG
fprintf(stderr,"valid_pt\n");
#endif DEBUG
#ifdef DEBUG_vp
fprintf(stderr,"valid_pt:  initial.arc=%d\n",initial.arc);
fprintf(stderr,"valid_pt:  present.arc=%d\n",present.arc);
fprintf(stderr,"valid_pt:  terminal.arc=%d\n",terminal.arc);
fprintf(stderr,"valid_pt:  arc_t_info.count=%d\n",arc_t_info.count);
#endif DEBUG_vp

  if ((arc_t_info.count!=1)||(initial.arc!=terminal.arc))
   {
    fprintf(stderr,"ERROR!!  Function:  \"valid_pt\" should not have been called!\n");
    sleep(5);
#ifdef DEBUG_vp
fprintf(stderr,"valid_pt: AWWWWW!!!!! valid_pt should NOT have been called\n");
#endif DEBUG_vp
    return(0);
   }

  dig_P_read_line(map,(abs(initial.arc)),&p);
/* beginning of segment */
  b_x = p->x[initial.segment-1]; 
  b_y = p->y[initial.segment-1]; 
/* end of segment */
  e_x = p->x[initial.segment]; 
  e_y = p->y[initial.segment]; 
  if (initial.arc>0)
   {
/* Initial.arc is going in a positive direction */
    distance(b_x,b_y,initial.e,initial.n,&dist);
    dist_b_i = dist;
    distance(b_x,b_y,pt_e,pt_n,&dist);
    dist_b_pt = dist;
    if (dist_b_pt>=dist_b_i)
     {
#ifdef DEBUG_vp
fprintf(stderr,"valid_pt:  dist_b_pt=%lf >= dist_b_i=%lf\n",dist_b_pt,dist_b_i);
#endif DEBUG_vp
      return(1);
     }
    else
     {
#ifdef DEBUG_vp
fprintf(stderr,"valid_pt:  dist_b_pt=%lf NOT >= dist_b_i=%lf\n",dist_b_pt,dist_b_i);
#endif DEBUG_vp
      return(0);
     }
   }
  else
   {
/* Initial.arc is going in a negative direction */
    distance(e_x,e_y,initial.e,initial.n,&dist);
    dist_e_i = dist;
    distance(e_x,e_y,pt_e,pt_n,&dist);
    dist_e_pt = dist;
    if (dist_e_pt>=dist_e_i)
     {
#ifdef DEBUG_vp
fprintf(stderr,"valid_pt:  dist_e_pt=%lf >= dist_e_i=%lf\n",dist_e_pt,dist_e_i);
#endif DEBUG_vp
      return(1);
     }
    else
     {
#ifdef DEBUG_vp
fprintf(stderr,"valid_pt:  dist_e_pt=%lf NOT >= dist_e_i=%lf\n",dist_e_pt,dist_e_i);
#endif DEBUG_vp
      return(0);
     }
   }
/* Should not reach here */
 }
