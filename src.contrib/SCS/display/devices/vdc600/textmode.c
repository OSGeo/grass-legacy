/* Function: textmode
**
** Author: Paul W. Carlson		Jan. 1990
*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

void textmode()
{
    ioctl(vidfd, SW_ENHC80x25, 0);
} 
