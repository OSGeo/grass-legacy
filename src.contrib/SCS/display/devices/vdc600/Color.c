/* Function: color		P.W. Carlson		1/90	*/

#include "vdc600.h"

color(number)
int number;
{
    /* set the current color */
    current.color = (unsigned char)number;
}
