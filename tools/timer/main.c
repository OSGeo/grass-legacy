/* prints the current time in seconds.milliseconds */

#include <sys/time.h>
#include <string.h>
#include <stdio.h>

int main()
{
   struct timeval t; 
          
   if (gettimeofday(&t, (struct timezone *)NULL) == -1) { 
      fprintf(stderr,"gettimeofday error"); 
      return 1; 
   } 
   fprintf(stdout,"%li.%li\n", t.tv_sec, t.tv_usec); 
   fflush(stdout);

   return 0; 
}
