/* Function: color		P.W. Carlson		April 1990	*/

#include "driver.h"

color(number)
int number;
{
extern int cur_color;
    static int no_dither[] = {
	0,1,1,1,1,   2,1,1,1,1,   2,2,1,1,1,   2,2,2,3,3,   2,2,3,3,3,
	4,5,5,5,5,   2,5,5,5,1,   2,2,3,3,3,   2,2,3,3,3,   2,2,3,3,3,
	4,5,5,5,5,   6,5,5,5,5,   6,6,5,5,5,   6,6,6,5,5,   6,6,6,3,7,
	4,5,5,5,5,   6,5,5,5,5,   6,6,5,5,5,   6,6,6,5,7,   6,6,6,7,7,
	4,4,5,5,5,   6,4,5,5,5,   6,6,5,5,5,   6,6,6,5,7,   6,6,6,7,7  };

    /* set the current color */
    cur_color = (unsigned char)number;

    /* pass the data to the device driver */
    put_chr('C');
    put_int(number);
    put_chr('c');
    put_int(no_dither[number]);
}
