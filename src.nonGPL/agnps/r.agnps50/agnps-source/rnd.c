#include <math.h>
#include "debugflg.h"

#ifdef _UNIX_K_AND_R
 
int rnd(number)
 
   float number;
 
#else
 
int rnd(float number)
 
#endif
 
{
int number_int;
 
 
    number_int = floor( number );
 
    if (number - number_int >= 0.5)
	number_int++;
 
    return (number_int);
}
 