/* Function: Erase		P.W. Carlson		1/90	*/

#include <memory.h>
#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

Erase()
{

    video.segment.number = 0;
    CHECK_SEG();
    memset(graphics_base, 0, 0x10000);
    video.segment.number++;
    CHECK_SEG();
    memset(graphics_base, 0, 0x10000);
    video.segment.number++;
    CHECK_SEG();
    memset(graphics_base, 0, 0x10000);
    video.segment.number++;
    CHECK_SEG();
    memset(graphics_base, 0, 0xE800);
}
