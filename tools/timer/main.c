/* prints the current time in milliseconds */

#include <sys/time.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h> 

int main()
{
   struct timeval *t; 
          
   if (gettimeofday(t,NULL) == -1) { 
      puts("gettimeofday error"); 
      return 1; 
   } 
   printf("%li.%li\n",t->tv_sec,t->tv_usec); 
   return 0; 
                                                                            
}
