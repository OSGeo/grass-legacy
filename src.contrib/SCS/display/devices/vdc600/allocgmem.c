/* Function: allocgmem
**
** Author: Paul W. Carlson		Jan. 1990
*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

void alloc_graphics_mem()
{
    /* get pointer to graphics memory */
    graphics_mem.vaddr = (char *)((unsigned long)malloc(0x11000) & 
			 0xFFFFF000) + 0x1000;
    graphics_mem.physaddr = (char *)0xA0000;
    graphics_mem.length = 0x10000;
    graphics_mem.ioflg = 1;
    ioctl(vidfd, KDMAPDISP, &graphics_mem);
    graphics_base = graphics_mem.vaddr;
}
