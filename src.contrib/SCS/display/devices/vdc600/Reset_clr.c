/* Function: reset_color	P.W. Carlson		1/90	*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

reset_color(position, red, green, blue)
int position;
unsigned char red, green, blue;
{
    red /= 4;
    green /= 4;
    blue /= 4;
    write_dac(position, red, green, blue);
}
