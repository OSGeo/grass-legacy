#include "graphics.h"

static int curx = 0;
static int cury = 0;

int move_abs (int x, int y)
{
    curx = x;
    cury = y;

    return 0;
}

int cont_abs (int x, int y)
{
    draw_line (curx, cury, x, y);
    curx = x;
    cury = y;

    return 0;
}
