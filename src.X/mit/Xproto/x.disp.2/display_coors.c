#include "gis.h"
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

extern struct Cell_head window;
extern unsigned disp_width, disp_height;
extern double e_minus_w, n_minus_s;
extern GC coor_gc;
extern Window coor_win;
extern int n_vects;
extern char *vector_files[5], cell_displayed[25];
extern Display *dpy;

void display_coors(disp, event, params, nparams)
Widget disp;
XEvent *event;
char **params;
int *nparams;
{
    extern double xl, xr, yt, yb;
    int x, y, easting, northing;
    char str[30];

    if (cell_displayed[0] == '\0' && n_vects == 0)
        return;

    x = (int)((XButtonPressedEvent *)event)->x;
    y = (int)((XButtonPressedEvent *)event)->y;
    if (x < ((int) xl) || x > ((int) xr) ||
			y < ((int) yt) || y > ((int) yb)) {
        XClearWindow(dpy, coor_win);
        return;
    }
    easting = (int) (window.west + (x - xl) * e_minus_w / (xr - xl));
    northing = (int) (window.north - (y - yt) * n_minus_s / (yb - yt));

    sprintf(str, "%8d %8d", easting, northing);
    XClearWindow(dpy, coor_win);
    XDrawString(dpy, coor_win, coor_gc, 4, 11, str, strlen(str));
    XFlush(dpy);
}

