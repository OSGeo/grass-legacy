
#include "gis.h"
#include "options.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>


#define  XADJ(U_X)  (int)((U_X - window->west ) * U_to_D_xconv + xl)
#define  YADJ(U_Y)  (int)((window->north - U_Y) * U_to_D_yconv + yt)

extern double xl, xr, yt, yb;
extern double U_to_D_xconv, U_to_D_yconv;

extern GC gc;
extern Display *dpy;
extern Window win;

char *fgets();

draw_points_diamond(size, window)
struct Cell_head *window;
{
    double U_X, U_Y;
    int D_X, D_Y;

    while (next_point(window, &U_X, &U_Y)) {
        D_X = XADJ(U_X);
        D_Y = YADJ(U_Y);

        XDrawLine(dpy, win, gc,
            D_X, D_Y + size, D_X + size, D_Y);

        XDrawLine(dpy, win, gc,
            D_X + size, D_Y, D_X, D_Y - size);

        XDrawLine(dpy, win, gc,
            D_X, D_Y - size, D_X - size, D_Y);

        XDrawLine(dpy, win, gc,
            D_X - size, D_Y, D_X, D_Y + size);

    }
}

draw_points_box(size, window)
struct Cell_head *window;
{
    double U_X, U_Y;
    int D_X, D_Y;

    while (next_point(window, &U_X, &U_Y)) {


        D_X = XADJ(U_X);
        D_Y = YADJ(U_Y);

        XDrawLine(dpy, win, gc,
            D_X - size, D_Y - size, D_X - size, D_Y + size);

        XDrawLine(dpy, win, gc,
            D_X - size, D_Y + size, D_X + size, D_Y + size);

        XDrawLine(dpy, win, gc,
            D_X + size, D_Y + size, D_X + size, D_Y - size);

        XDrawLine(dpy, win, gc,
            D_X + size, D_Y - size, D_X - size, D_Y - size);

    }
}

draw_points_plus(size, window)
struct Cell_head *window;
{
    double U_X, U_Y;
    int D_X, D_Y;

    while (next_point(window, &U_X, &U_Y)) {

        /* printf("\n xl=%lf, conv=%lf, west=%lf", xl, U_to_D_xconv,
         * window->west);
         * 
         * 
         * printf("\n ux = %lf, uy = %lf", U_X, U_Y); */

        D_X = XADJ(U_X);
        D_Y = YADJ(U_Y);

        /* printf("\n x = %d, y = %d, size = %d", D_X, D_Y, size); */

        XDrawLine(dpy, win, gc,
            D_X, D_Y - size, D_X, D_Y + size);
        XDrawLine(dpy, win, gc,
            D_X - size, D_Y, D_X + size, D_Y);

    }
}



draw_points_x(size, window)
struct Cell_head *window;
{
    double U_X, U_Y;
    int D_X, D_Y;

    while (next_point(window, &U_X, &U_Y)) {
        D_X = XADJ(U_X);
        D_Y = YADJ(U_Y);

        XDrawLine(dpy, win, gc,
            D_X - size, D_Y - size, D_X + size, D_Y + size);

        XDrawLine(dpy, win, gc,
            D_X - size, D_Y + size, D_X + size, D_Y - size);
    }
}


next_point(window, U_X, U_Y)
struct Cell_head *window;
double *U_X;
double *U_Y;
{
    char buffer[128];

    do {
        if (!fgets(buffer, sizeof buffer, infile))
            return 0;
        if (sscanf(buffer, "%lf %lf", U_X, U_Y) != 2)
            return 0;
    }
    while (*U_X < window->west || *U_X > window->east ||
        *U_Y < window->south || *U_Y > window->north);

    return 1;
}
