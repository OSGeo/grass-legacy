/* "valid_seg" will only be called if arc_t_info.count equals 1 and          */
/* abs(initial.arc) equals abs(present.arc)   or                             */
/* abs(initial.arc) equals abs(terminal.arc)                                 */
/* This function will determine if variable "segment" is a valid segment     */
/* or not.                                                                   */
#include "distance.h"
int valid_seg(segment)
 int segment;
 {

  if ((abs(initial.arc)!=abs(present.arc)) &&
      (abs(initial.arc)!=abs(terminal.arc))  )
   {
    fprintf(stderr,"ERROR!!  Function:  \"valid_seg\" should not have been called!\n");
    sleep(5);
    return(0);
   }

  if (initial.arc>0)
   {
/* Arc is going in a positive direction */
    if (initial.indicator=='4')
     {
/* When initial.indicator equals 4 */
      if (segment>initial.segment)
       {
        return(1);
       }
      else
       {
        return(0);
       }
     }
    else
     {
      if (initial.indicator=='1'||initial.indicator=='3'||initial.indicator=='5')
       {
/* When initial.indicator equals 1, 3, or 5 */
        if (segment>=initial.segment)
         {
          return(1);
         }
        else
         {
          return(0);
         }
       }
      else
       {
/* Initial.indicator should not be 2 */
        return(0);
       }
     }
   }
  else
   {
/* Arc is going in a negative direction */
    if (initial.indicator=='3')
     {
/* When initial.indicator equals 3 */
      if (segment<initial.segment)
       {
        return(1);
       }
      else
       {
        return(0);
       }
     }
    else
     {
      if (initial.indicator=='2'||initial.indicator=='4'||initial.indicator=='5')
       {
/* When initial.indicator equals 2, 4, or 5 */
        if (segment<=initial.segment)
         {
          return(1);
         }
        else
         {
          return(0);
         }
       }
      else
       {
/* Initial.indicator should not be 1 */
        return(0);
       }
     }
   }
/* Should not reach here */
 }
