#include "graphics.h"
set_width (n)
{
    if (n <= 0) n = 1;
    graphics.width1 = n-- / 2;
    graphics.width2 = n - graphics.width1;
}
