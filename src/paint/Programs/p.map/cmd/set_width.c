#include "graphics.h"

int set_width (int n)
{
    if (n <= 0) n = 1;
    graphics.width1 = n-- / 2;
    graphics.width2 = n - graphics.width1;

    return 0;
}
