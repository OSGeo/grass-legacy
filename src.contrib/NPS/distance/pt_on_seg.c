#include "distance.h"
#include <math.h>
/****************************************************************************/
/* Variable:       Definition:                                              */
/* seg_ind         If equal to "B" then the beginning point of the segment. */
/*                 If equal to "E" then the ending point of the segment.    */
/*                 If equal to "P" then the segment is a point.             */
/* seg_type        If equal to "V" then segment is a vertical line.         */
/*                 If equal to "H" then segment is a horizontal line.       */
/*                 If equal to "N" then segment is neither a horizontal     */
/*                 line nor a vertical line.                                */ 
/* x1a             X value of beginning point of segment.                   */
/* y1a             Y value of beginning point of segment.                   */
/* x1b             X value of ending point of segment.                      */
/* y1b             Y value of ending point of segment.                      */
/* x_pt            X value for point at cross-hairs produced by mouse click.*/
/* y_pt            Y value for point at cross-hairs produced by mouse click.*/
/* x               X value of point on segment that is the closest point to */
/*                 point (x_pt,y_pt).                                       */
/* y               Y value of point on segment that is the closest point to */
/*                 point (x_pt,y_pt).                                       */
/* dist            Distance between point (x_pt,y_pt) and point (x,y).      */
/****************************************************************************/
int
pt_on_seg(seg_ind,seg_type,x1a,y1a,x1b,y1b,x_pt,y_pt,x,y,dist)    
 char *seg_ind;
 char *seg_type;
 double x1a,y1a,x1b,y1b,x_pt,y_pt,*x,*y;
 double *dist;
 {
  double slope,b;
  double a1,b1,c1,a2,b2,c2,d2;
  double d,dx,dy;
  double x_e,y_e,x_f,y_f;
  double pt_ae,pt_bf,b_ae,a_bf;
  double c3,c4;
  int check_sign();

#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: x1a=%lf y1a=%lf x1b=%lf y1b=%lf;  x_pt=%lf y_pt=%lf\n",
    x1a,y1a,x1b,y1b,x_pt,y_pt);
#endif DEBUG_pos
  if (x1a==x1b)
   {
    if (y1a==y1b)
     {
/* Segment 1 is a single "point".                                            */
      *x = x1a;
      *y = y1a;
      *seg_ind = 'P';
      *seg_type = 'P';
      distance(x_pt,y_pt,*x,*y,dist);
      return(1);
     }
/* Segment 1 is a vertical line.                                             */
    *seg_type = 'V';
    if ((y_pt<=y1a&&y_pt>=y1b)||(y_pt>=y1a&&y_pt<=y1b))
     {
      *x = x1a;
      *y = y_pt; 
      if (y_pt == y1a)
       *seg_ind = 'B';
      else
       if (y_pt == y1b)
         *seg_ind = 'E';
       else
         *seg_ind = 'M';
     }
    else
     {
      if ( (pow((x1a-x_pt),(double)2.0)+pow((y1a-y_pt),(double)2.0))<
           (pow((x1b-x_pt),(double)2.0)+pow((y1b-y_pt),(double)2.0)) )
       {
        *seg_ind = 'B';
        *x = x1a; 
        *y = y1a;
       }
      else
       {
        *seg_ind = 'E';
        *x = x1b; 
        *y = y1b;
       }
     }
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: segment 1 is a vertical line\n");
fprintf(stderr,"pt_on_seg: *seg_type=%c *seg_ind=%c *x=%lf *y=%lf\n",*seg_type,*seg_ind,*x,*y);
#endif DEBUG_pos
   }
  else
   {
    if (y1a==y1b)
     {
/* Segment 1 is a horizontal line.                                           */
      *seg_type = 'H';
      if ((x_pt<=x1a&&x_pt>=x1b)||(x_pt>=x1a&&x_pt<=x1b))
       {
        *x = x_pt;
        *y = y1a; 
        if (x_pt == x1a)
         *seg_ind = 'B';
        else
         if (x_pt == x1b)
           *seg_ind = 'E';
         else
           *seg_ind = 'M';
       }
      else
       {
        if ( (pow((x1a-x_pt),(double)2.0)+pow((y1a-y_pt),(double)2.0))<
             (pow((x1b-x_pt),(double)2.0)+pow((y1b-y_pt),(double)2.0)) )
         {
          *seg_ind = 'B';
          *x = x1a; 
          *y = y1a;
         }
        else
         {
          *seg_ind = 'E';
          *x = x1b; 
          *y = y1b;
         }
       }
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: segment 1 is a horizontal line\n");
fprintf(stderr,"pt_on_seg: *seg_type=%c *seg_ind=%c *x=%lf *y=%lf\n",*seg_type,*seg_ind,*x,*y);
#endif DEBUG_pos
     }
    else
     {
/* Segment 1 is neither a vertical line nor a horizontal line.               */
      *seg_type = 'N';
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: *seg_type=%c\n",*seg_type);
#endif DEBUG_pos

/* Determine if beginning or ending point of segment 1 occurs on y-axis. */
      if ((x1a==0.0)||(x1b==0.0))
       {
/* Beginning or ending point of segment 1 occurs on y-axis.                  */
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: beginning or ending point of segment 1 occurs on y-axis\n");
#endif DEBUG_pos
        a2 = (-1.0)/((y1b-y1a)/(x1b-x1a));
        if (x1a==0.0)
         {
/* Beginning point of segment 1 is on y-axis.                                */
/*****************************************************************************
                                    Y-Axis
                                    |       / (x1b,y1b)
           Beginning point of       |      /
           segment on Y-Axis.       |     /
           ("x1a" equals zero)      |    /
                                    |   /
                                    |  /
                                    | /
                                    |/ (x1a,y1a)
                                    |
                                    |
         ---------------------------|------------------------- X-Axis
                                    |
 *****************************************************************************/
          if (x1b<0.0)
            x_e =  1000.0;
          else
            x_e = -1000.0;
/* Create point "e".                                                         */
/* "c3" is the "y-intercept".                                                */
          c3  =  y1a - (a2*x1a);
          y_e = (a2*x_e)+c3;
/* Create point "f".                                                         */
/* "c4" is the "y-intercept".                                                */
          c4 =  y1b - (a2*x1b);
          y_f = c4;
          x_f = 0.0;
         }
        else
         {
          if (x1b==0.0)
           {
/* Ending point of segment 1 is on y-axis.                                   */
/*****************************************************************************
                                    Y-Axis
                                    |       / (x1a,y1a)
           Ending point of          |      /
           segment on Y-Axis.       |     /
           ("x1b" equals zero)      |    /
                                    |   /
                                    |  /
                                    | /
                                    |/ (x1b,y1b)
                                    |
                                    |
         ---------------------------|------------------------- X-Axis
                                    |
 *****************************************************************************/
/* Create point "e".                                                         */
/* "c3" is the "y-intercept".                                                */
            c3 =  y1a - (a2*x1a);
            y_e = c3;
            x_e = 0.0;
/* Create point "f".                                                         */
/* "c4" is the "y-intercept".                                                */
            if (x1a<0.0)
              x_f =  1000.0;
            else
              x_f = -1000.0;
            c4 =  y1b - (a2*x1b);
            y_f = (a2*x_f)+c4;
           }
         }
       }
      else
       {
/* Neither beginning or ending point of segment 1 occur on y-axis.           */
        a2 = (-1.0)/((y1b-y1a)/(x1b-x1a));
/* Create point "e".                                                         */
/* "c3" is the "y-intercept".                                                */
        c3 =  y1a - (a2*x1a);
        y_e = c3;
        x_e = 0.0;
/* Create point "f".                                                         */
/* "c4" is the "y-intercept".                                                */
        c4 =  y1b - (a2*x1b);
        y_f = c4;
        x_f = 0.0;
       }
/* "pt_ae" is (x_pt,y_pt) in relation to segment a-e.                        */
      pt_ae = (x_e-x1a)*(y_pt-y1a)-(y_e-y1a)*(x_pt-x1a);
/* "pt_bf" is (x_pt,y_pt) in relation to segment b-f.                        */
      pt_bf = (x_f-x1b)*(y_pt-y1b)-(y_f-y1b)*(x_pt-x1b);
/* "b_ae" is point "b" of segment 1 in relation to segment a-e.              */
      b_ae  = (x_e-x1a)*(y1b-y1a)- (y_e-y1a)*(x1b-x1a);
/* "a_bf" is point "a" of segment 1 in relation to segment b-f.              */
      a_bf  = (x_f-x1b)*(y1a-y1b)- (y_f-y1b)*(x1a-x1b);
/* Determine if segment 1 crosses the y-axis.                                */
      if ( ((x1a>0.0)&&(x1b<0.0))||((x1b>0.0)&&(x1a<0.0)) )
       {
/* Segment 1 crosses the y-axis.                                             */
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: segment 1 crosses the y-axis\n");
#endif DEBUG_pos
        a1 = (y1a-y1b) / (x1a-x1b);
        if (a1<0.0)
         {
/* Slope "a1" is negative.                                                   */
/*****************************************************************************
                                    Y-Axis
                                    |
                      +            /| e
                                 /  |
                               /    |
                             /      |           Line a-e is perpendicular
                           /        |           to line a-b.
                         /       -  |
                       /            |
                  a  /              |
           (x1a,y1a) \              |      Line Segment 1 crosses the Y-Axis
                       \            |
                         \          |
                           \        |
                             \      |
                               \    |
                                 \  |
                                   \|
                                    |\   Line Segment 1 (with negative slope)
                                    |  \
                                    |    \
                                    |      \
        Line b-f is perpendicular   |        \   b
        to line a-b.                |          \ (x1b,y1b)
                                    |         /
                                    |  -    /
                                    |     /
                                    |   /     +
                                  f | /
                                    |
         ---------------------------|------------------------- X-Axis
                                    |
 *****************************************************************************/
          if (pt_ae<0.0&&pt_bf<0.0)
           {
/* Point (*x,*y) is a mid-point on segment 1.                                */
            *seg_ind = 'M';
            a1 = (y1a-y1b) / (x1a-x1b);
            b1 = 1.0;
            c1 = y1a - (a1*x1a);
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: a1=%lf b1=%lf c1=%lf\n",a1,b1,c1);
#endif DEBUG_pos
/* "a2" is perpendicular slope to slope "a1".                                */
            a2 = -(1.0/a1);
            b2 = 1.0;
            c2 = y_pt - (a2*x_pt);
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: a2=%lf b2=%lf c2=%lf\n",a2,b2,c2);
#endif DEBUG_pos
            d  = (a1*b2) - (a2*b1);
            dx = (c1*b2) - (c2*b1);
            dy = (a1*c2) - (a2*c1);
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: dx=%lf dy=%lf d=%lf\n",dx,dy,d);
#endif DEBUG_pos
            *x  = dx / d;
            *y  = dy / d;
            return( check_sign(x1a,y1a,x1b,y1b,x,y,x_pt,y_pt,dist) );
           }
          else
           {
            if (pt_ae>=0.0&&pt_bf<0.0)
             {
/* Beginning point of segment 1 is point (*x,*y).                            */
              *x = x1a;
              *y = y1a;
              *seg_ind = 'B';
              distance(x_pt,y_pt,*x,*y,dist);
              return(1);
             }
            else
             {
              if (pt_bf>=0.0&&pt_ae<0.0)
               {
/* Ending point of segment 1 is point (*x,*y).                               */
                *x = x1b;
                *y = y1b;
                *seg_ind = 'E';
                distance(x_pt,y_pt,*x,*y,dist);
                return(1);
               }
              else
               {
/* Should not reach here!                                                    */
                fprintf(stderr,"1:  ERROR in function \"pt_on_seg\".\n"); 
                return(0);
               }
             }
           }
         }
        else
         {
          if (a1>0.0)
           {
/* Slope "a1" is positive.                                                   */
/*****************************************************************************
                                    Y-Axis
                                  e |
                                    |\
                                    |  \
                                    |    \     -
      Line a-e is perpendicular     |      \
      to line a-b.                  |  +     \
                                    |          \  a  
                                    |           / (x1a,y1a)
                                    |         /
Line Segment 1 crosses the Y-Axis   |       /
                                    |     /
                                    |   /  Line Segment 1 (with positive slope)
                                    | /   
                                    |
                                  / |
                                /   |
                              /     |
                            /       |
                        b /         |
                 (x1b,y1b)\         |            Line b-f is perpendicular
                            \   +   |            to line a-b.
                              \     |
                           -    \   |
                                  \ | f
                                    |
         ---------------------------|------------------------- X-Axis
                                    |
 *****************************************************************************/
            if (pt_ae>0.0&&pt_bf>0.0)
             {
/* Point (*x,*y) is a mid-point on segment 1.                                */
              *seg_ind = 'M';
              a1 = (y1a-y1b) / (x1a-x1b);
              b1 = 1.0;
              c1 = y1a - (a1*x1a);
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: a1=%lf b1=%lf c1=%lf\n",a1,b1,c1);
#endif DEBUG_pos
/* "a2" is perpendicular slope to slope "a1".                                */
              a2 = -(1.0/a1);
              b2 = 1.0;
              c2 = y_pt - (a2*x_pt);
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: a2=%lf b2=%lf c2=%lf\n",a2,b2,c2);
#endif DEBUG_pos
              d  = (a1*b2) - (a2*b1);
              dx = (c1*b2) - (c2*b1);
              dy = (a1*c2) - (a2*c1);
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: dx=%lf dy=%lf d=%lf\n",dx,dy,d);
#endif DEBUG_pos
              *x  = dx / d;
              *y  = dy / d;
              return( check_sign(x1a,y1a,x1b,y1b,x,y,x_pt,y_pt,dist) );
             }
            else
             {
              if (pt_ae<=0.0&&pt_bf>0.0)
               {
/* Beginning point of segment 1 is point (*x,*y).                            */
                *x = x1a;
                *y = y1a;
                *seg_ind = 'B';
                distance(x_pt,y_pt,*x,*y,dist);
                return(1);
               }
              else
               {
                if (pt_bf<=0.0&&pt_ae>0.0)
                 {
/* Ending point of segment 1 is point (*x,*y).                               */
                  *x = x1b;
                  *y = y1b;
                  *seg_ind = 'E';
                  distance(x_pt,y_pt,*x,*y,dist);
                  return(1);
                 }
                else
                 {
/* Should not reach here!                                                    */
                  fprintf(stderr,"2:  ERROR in function \"pt_on_seg\".\n"); 
                  return(0);
                 }
               }
             }
           }
         }
       }
      else
       {
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: segment 1 does NOT cross the y-axis\n");
#endif DEBUG_pos
/* Segment 1 does not cross y-axis if you reach this point.                  */
/*****************************************************************************
                                    Y-Axis
                                    |
  Segment 1 does NOT cross          |       a 
  the Y-Axis                        | -    /\ (x1a,y1a)
                                    |    /    \
                                    |  /        \
                                  e |/    +       \
                                    |               \
    Line a-e is perpendicular       |                 \ b
    to line a-b.                    |                /  (x1b,y1b)
                                    |              /
                                    |            /
                                    |          /    
                                    |    -   /      Line b-f is perpendicular
                                    |      /        to line a-b.
                                    |    /   +
                                    |  /
                                  f |/
                                    |
         ---------------------------|------------------------- X-Axis
                                    |
 *****************************************************************************/
        a1 = (y1a-y1b) / (x1a-x1b);
        if ((pt_ae<0.0&&pt_bf>0.0)||(pt_ae>0.0&&pt_bf<0.0))
/* Point (*x,*y) is on segment 1 between points "a" and "b".                 */
         { 
          *seg_ind = 'M';
          a1 = (y1a-y1b) / (x1a-x1b);
          b1 = 1.0;
          c1 = y1a - (a1*x1a);
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: a1=%lf b1=%lf c1=%lf\n",a1,b1,c1);
#endif DEBUG_pos
/* "a2" is perpendicular slope to slope "a1".                                */
          a2 = -(1.0/a1);
          b2 = 1.0;
          c2 = y_pt - (a2*x_pt);
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: a2=%lf b2=%lf c2=%lf\n",a2,b2,c2);
#endif DEBUG_pos
          d  = (a1*b2) - (a2*b1);
          dx = (c1*b2) - (c2*b1);
          dy = (a1*c2) - (a2*c1);
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: dx=%lf dy=%lf d=%lf\n",dx,dy,d);
#endif DEBUG_pos
          *x  = dx / d;
          *y  = dy / d;
          return( check_sign(x1a,y1a,x1b,y1b,x,y,x_pt,y_pt,dist) );
         }
        else
         { 
          if (  (pt_ae<=0.0&&pt_bf<0.0&&a_bf<0.0&&b_ae>0.0)
              ||(pt_ae>=0.0&&pt_bf>0.0&&a_bf>0.0&&b_ae<0.0) )
/* Point (x_pt,y_pt) is NOT a mid-point on segment 1 and is closest to the   */
/* begining point of segment 1.                                              */
           {
            *seg_ind = 'B';
            *x = x1a;
            *y = y1a;
            distance(x_pt,y_pt,*x,*y,dist);
            return(1);
           }
          else
           {
            if (  (pt_ae>0.0&&pt_bf>=0.0&&a_bf<0.0&&b_ae>0.0)
                ||(pt_ae<0.0&&pt_bf<=0.0&&a_bf>0.0&&b_ae<0.0) )
/* Point (x_pt,y_pt) is NOT mid-point on segment 1 and is closest to the     */
/* ending point of segment 1.                                                */
             {
              *seg_ind = 'E';
              *x = x1b;
              *y = y1b;
              distance(x_pt,y_pt,*x,*y,dist);
              return(1);
             }
            else
             {
/* Should not reach here!                                                    */
              fprintf(stderr,"3:  ERROR in function \"pt_on_seg\".\n"); 
              return(0);
             }
           }
         }
       }
#ifdef DEBUG_pos
fprintf(stderr,"pt_on_seg: *seg_ind=%c *x=%lf *y=%lf\n",*seg_ind,*x,*y);
#endif DEBUG_pos
     }
   }
/* Should not reach here!                                                    */
  return(0);
 }



#include <stdio.h>
int
check_sign(x1a,y1a,x1b,y1b,x,y,x_pt,y_pt,dist)
 double x1a,y1a,x1b,y1b;
 double *x,*y;
 double x_pt,y_pt,*dist;
 { 
  double a1,c1;
  static double tolerance = {0.00001};
  double temp;

  a1 = (y1a-y1b) / (x1a-x1b);
  c1 = y1a - (a1*x1a);
/* Try "*x" and "*y" as is and then see if the equation equals "c1".         */
  temp = ((*y)-(a1*(*x)));
  if ( (temp>=(c1-tolerance))&&(temp<=(c1+tolerance)) )
   {
#ifdef DEBUG_cs
fprintf(stderr,"check_sign: *x and *y work as is\n");
fprintf(stderr,"check_sign: c1-tolerance=%35.32lf\n",c1-tolerance);
fprintf(stderr,"check_sign: temp        =%35.32lf\n",temp);
fprintf(stderr,"check_sign: c1+tolerance=%35.32lf\n",c1+tolerance);
#endif DEBUG_cs
/* "*x" and "*y" are correct.                                                */
    distance(x_pt,y_pt,*x,*y,dist);
    return(1);
   }
  else
   {
/* Try changing the sign for "*x".                                           */
    *x = -(*x);
    temp = ((*y)-(a1*(*x)));
#ifdef DEBUG_cs
fprintf(stderr,"check_sign: change sign of *x to: *x=%lf\n",*x);
fprintf(stderr,"check_sign: c1-tolerance=%35.32lf\n",c1-tolerance);
fprintf(stderr,"check_sign: temp        =%35.32lf\n",temp);
fprintf(stderr,"check_sign: c1+tolerance=%35.32lf\n",c1+tolerance);
#endif DEBUG_cs
    if ( (temp>=(c1-tolerance))&&(temp<=(c1+tolerance)) )
     {
#ifdef DEBUG_cs
fprintf(stderr,"check_sign: sign changed for *x to make it work\n");
fprintf(stderr,"check_sign: c1-tolerance=%35.32lf\n",c1-tolerance);
fprintf(stderr,"check_sign: temp        =%35.32lf\n",temp);
fprintf(stderr,"check_sign: c1+tolerance=%35.32lf\n",c1+tolerance);
#endif DEBUG_cs
/* "*x" and "*y" are correct.                                                */
      distance(x_pt,y_pt,*x,*y,dist);
      return(1);
     }
    else
     {
/* Change back "*x" to original sign.                                        */
      *x = -(*x);
/* Now change "*y" to opposite sign and try in equation.                     */
      *y = -(*y);
      temp = ((*y)-(a1*(*x)));
#ifdef DEBUG_cs
fprintf(stderr,"check_sign: change sign of *y to: *y=%lf\n",*y);
fprintf(stderr,"check_sign: c1-tolerance=%35.32lf\n",c1-tolerance);
fprintf(stderr,"check_sign: temp        =%35.32lf\n",temp);
fprintf(stderr,"check_sign: c1+tolerance=%35.32lf\n",c1+tolerance);
#endif DEBUG_cs
      if ( (temp>=(c1-tolerance))&&(temp<=(c1+tolerance)) )
       {
#ifdef DEBUG_cs
fprintf(stderr,"check_sign: sign changed for *y to make it work\n");
fprintf(stderr,"check_sign: c1-tolerance=%35.32lf\n",c1-tolerance);
fprintf(stderr,"check_sign: temp        =%35.32lf\n",temp);
fprintf(stderr,"check_sign: c1+tolerance=%35.32lf\n",c1+tolerance);
#endif DEBUG_cs
/* "*x" and "*y" are correct.                                                */
        distance(x_pt,y_pt,*x,*y,dist);
        return(1);
       }
      else
       {
/* Change back "*y" to original sign.                                        */
        *y = -(*y);
/* Now change both "*x" and "*y" to opposite sign and try in equation.       */
        *x = -(*x);
        *y = -(*y);
        temp = ((*y)-(a1*(*x)));
#ifdef DEBUG_cs
fprintf(stderr,"check_sign: change sign of *y to: *y=%lf and *x to *x=%lf\n",*y,*x);
fprintf(stderr,"check_sign: c1-tolerance=%35.32lf\n",c1-tolerance);
fprintf(stderr,"check_sign: temp        =%35.32lf\n",temp);
fprintf(stderr,"check_sign: c1+tolerance=%35.32lf\n",c1+tolerance);
#endif DEBUG_cs
        if ( (temp>=(c1-tolerance))&&(temp<=(c1+tolerance)) )
         {
#ifdef DEBUG_cs
fprintf(stderr,"check_sign: signs changed for both *x and *y to make it work\n");
fprintf(stderr,"check_sign: c1-tolerance=%35.32lf\n",c1-tolerance);
fprintf(stderr,"check_sign: temp        =%35.32lf\n",temp);
fprintf(stderr,"check_sign: c1+tolerance=%35.32lf\n",c1+tolerance);
#endif DEBUG_cs
/* "*x" and "*y" are correct.                                                */
          distance(x_pt,y_pt,*x,*y,dist);
          return(1);
         }
        else
         {
#ifdef DEBUG_cs
fprintf(stderr,"check_sign: Unsuccessful in calculating *x and *y\n");
fprintf(stderr,"check_sign: c1-tolerance=%35.32lf\n",c1-tolerance);
fprintf(stderr,"check_sign: temp        =%35.32lf\n",temp);
fprintf(stderr,"check_sign: c1+tolerance=%35.32lf\n",c1+tolerance);
#endif DEBUG_cs
/* Should not reach here!                                                    */
          fprintf(stderr,"ERROR in function \"check_sign\".\n"); 
          fprintf(stderr,"Unsuccessful in calculating \"*x\" and \"*y\".\n");
          return(0);
         }
       }
     }
   }
/* Should not reach here!                                                    */
 }
