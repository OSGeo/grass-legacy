#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

Screen_left(index)
int *index;
{
    *index = SCREEN_LEFT;
}


Screen_rite(index)
int *index;
{
    *index = SCREEN_RIGHT;
}


Screen_bot(index)
int *index;
{
    *index = SCREEN_BOTTOM;
}


Screen_top(index)
int *index;
{
    *index = SCREEN_TOP;
}


Get_num_colors(index)
int *index;
{
    *index = NCOLORS;
}


get_num_colors()
{
    return NCOLORS;
}
