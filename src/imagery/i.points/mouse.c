#include "globals.h"

static int first = 1;
static int curx, cury;

Mouse_pointer (x, y, button)
    int *x, *y, *button;
{
    if (first)
    {
	curx = (SCREEN_LEFT + SCREEN_RIGHT)/2;
	cury = (SCREEN_TOP + SCREEN_BOTTOM)/2;
	first = 0;
    }
    R_get_location_with_pointer (&curx, &cury, button);
    *x = curx;
    *y = cury;

#ifdef BUTTON3
    if (*button == 3) quit(0);
#endif
}

Mouse_box_anchored (x1, y1, x2, y2, button)
    int *x2, *y2, *button;
{
    R_get_location_with_box (x1, y1, x2, y2, button);
    curx = *x2;
    cury = *y2;
    first = 0;

#ifdef BUTTON3
    if (*button == 3) quit(0);
#endif
}

Get_mouse_xy(x,y)
    int *x, *y;
{
    *x = curx;
    *y = cury;
}

Set_mouse_xy(x,y)
{
    first = 0;
    curx = x;
    cury = y;
}
