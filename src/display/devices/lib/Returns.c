/* These return the values as defined in the Graph_Set() routine */

#include "driver.h"

int Screen_left (int *index)
{
    *index = screen_left;

    return 0;
}

int Screen_rite (int *index)
{
    *index = screen_right;

    return 0;
}

int Screen_bot (int *index)
{
    *index = screen_bottom;

    return 0;
}

int Screen_top (int *index)
{
    *index = screen_top;

    return 0;
}

int Get_num_colors (int *ncolors)
{
    *ncolors = NCOLORS;

    return 0;
}

