/******************************************************************************

There is a four bit "outcode" based on the nine regions.  Each bit in the 
"outcode" is set to 1 (TRUE) if a given relation between the end point and  
window is true:
                    Bit 1 -- point is above window.
                    Bit 2 -- point is below window.
                    Bit 3 -- point is right of window.
                    Bit 4 -- point is left of window.

otherwise bit is 0 (FALSE).


                        Outcodes for endpoints

                             |         |
                       1001  |  1000   |  1010
                             |         |
                    _________|_________|_________
                             |         |
                       0001  |  0000   |  0010
                             |         |
                    _________|_________|_________
                             |         |
                       0101  |  0100   |  0110
                             |         |
                             |         |

Variables "outcode1" and "outcode2" are the "outcodes" for both of the endpoints
of the line segment.

******************************************************************************/
static short int outcodes (double, double, double, double, double, double);
static short int reject_check (int, int);
static short int accept_check (int, int);

short int clip (register short int *accept,
   register double *x1, register double *y1,
   register double *x2, register double *y2,
   register double xmin, register double xmax,
   register double ymin, register double ymax)
 { 
 register short int done;
 register short int reject;
 short int swap;
 register double ftemp;
 static short int mask1000 = {8};
 static short int mask0100 = {4};
 static short int mask0010 = {2};
 static short int mask0001 = {1};
 register short int outcode1;
 register short int outcode2;
 register short int itemp;

/* Initialize values for function */
 *accept = 0;
 done = 0;
 reject = 0;
 swap = 0;

 while (done == 0)
  { 
   outcode1 = outcodes(*x1,*y1,xmin,xmax,ymin,ymax);
   outcode2 = outcodes(*x2,*y2,xmin,xmax,ymin,ymax);
   reject = reject_check(outcode1,outcode2);
/* If both end points are complelely outside the window area then
   "reject" will equal 1 else "reject" will equal 0 */
   if (reject==1)
     done = 1;
   else
    {
/* If both end points are completely inside the window area then
   "accept" will equal 1 else "accept" will equal 0 */
     *accept = accept_check(outcode1,outcode2);
     if (*accept==1)
       done = 1;
     else
      {
/*Variable "reject" is not equal to 1 and variable "accept" is not equal to 1 */
/* If point 1 is inside window, exchange points 1 and 2 and their outcodes
to guarantee that point 1 is outside "window"  */
       if (outcode1==0)
        {
         ftemp=(*x1), (*x1)=(*x2), (*x2)=ftemp; 
         ftemp=(*y1), (*y1)=(*y2), (*y2)=ftemp; 
         itemp=outcode1, outcode1=outcode2, outcode2=itemp;
         swap = 1;
        }
/* Divide line at top of window */
       if ( ((outcode1&mask1000)==8) )
        {
         *x1 = *x1 + (*x2 - *x1) * (ymax - *y1) / (*y2 - *y1);
         *y1 = ymax;
        }
       else
/* Divide line at bottom of window */
         if ( ((outcode1&mask0100)==4) )
          {
           *x1 = *x1 + (*x2 - *x1) * (ymin - *y1) / (*y2 - *y1);
           *y1 = ymin;
          }
         else
/* Divide line at right edge of window */
           if ( ((outcode1&mask0010)==2) )
            {
             *y1 = *y1 + (*y2 - *y1) * (xmax - *x1) / (*x2 - *x1);
             *x1 = xmax;
            }
           else
/* Divide line at left edge of window */
             if ( ((outcode1&mask0001)==1) )
              {
               *y1 = *y1 + (*y2 - *y1) * (xmin - *x1) / (*x2 - *x1);
               *x1 = xmin;
              }
      }
    }
  }
/* If we reach here and "accept" = 1, then the line from (x1,y1) 
   to (x2,y2) is visible within the "window" */
  if (*accept==1) 
   {
    if (swap)
     {
      ftemp=(*x1), (*x1)=(*x2), (*x2)=ftemp; 
      ftemp=(*y1), (*y1)=(*y2), (*y2)=ftemp; 
     }
   }

   return 0;
 }

       
static short int outcodes (double x, double y, double xmin, double xmax,
  double ymin, double ymax)
 {
  register short int outcode;
 
  outcode = 0;
  if ((ymax-y)<0)
    outcode += 8;
  if ((y-ymin)<0)
    outcode += 4;
  if ((xmax-x)<0)
    outcode += 2;
  if ((x-xmin)<0)
    outcode += 1;
  return (outcode);
 }


static short int reject_check (int outcode1, int outcode2)
 {
  short int reject;
  static short int mask1000 = {8};
  static short int mask0100 = {4};
  static short int mask0010 = {2};
  static short int mask0001 = {1};
  register short int bit1_1;
  register short int bit2_1;
  register short int bit3_1;
  register short int bit4_1;
  register short int bit1_2;
  register short int bit2_2;
  register short int bit3_2;
  register short int bit4_2;

  bit1_1 = outcode1 & mask1000;
  bit2_1 = outcode1 & mask0100;
  bit3_1 = outcode1 & mask0010;
  bit4_1 = outcode1 & mask0001;

  bit1_2 = outcode2 & mask1000;
  bit2_2 = outcode2 & mask0100;
  bit3_2 = outcode2 & mask0010;
  bit4_2 = outcode2 & mask0001;

  if  ( ((bit1_1 & mask1000)==8)&&((bit1_2 & mask1000)==8) )
   {
    reject = 1;
    return(reject); 
   }
  if  ( ((bit2_1 & mask0100)==4)&&((bit2_2 & mask0100)==4) )
   {
    reject = 1;
    return(reject); 
   }
  if  ( ((bit3_1 & mask0010)==2)&&((bit3_2 & mask0010)==2) )
   {
    reject = 1;
    return(reject); 
   }
  if  ( ((bit4_1 & mask0001)==1)&&((bit4_2 & mask0001)==1) )
   {
    reject = 1;
    return(reject); 
   }
  reject = 0;
  return(reject);
 }


static short int accept_check (int outcode1, int outcode2)
 {
  short int accept;

  if ((outcode1==0)&&(outcode2==0))
    accept = 1;
  else
    accept = 0;
  return(accept);
 }
