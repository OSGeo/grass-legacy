#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "options.h"

extern GC gc;
extern Display *dpy;
extern Window win;
extern int scrn;
extern Colormap colormap;

#define CHUNK   128

static int coors_allocated = 0;
static int *xarray;
static int *yarray;
static float xincr;
static float yincr;
XPoint *points;
char *falloc();
char *frealloc();

set_graph_stuff(window_width, window_height)
int window_width, window_height;
{
    xincr = (float) window_width / 100.;
    yincr = (float) window_height / 100.;
}


set_text_size()
{
    if (hsize >= 0.
        && vsize >= 0.
        && hsize <= 100.
        && vsize <= 100.)
        Text_size((int) (hsize * xincr), (int) (vsize * yincr));
    return (0);
}

do_draw(buff)
char *buff;
{
    float xper, yper;

    if (2 != sscanf(buff, "%*s %f %f", &xper, &yper))
        return (-1);
    if (xper < 0.0  || yper < 0.0  || xper > 100.0  || yper > 100.0)
        return (-1);

    Cont_abs(l + (int) (xper * xincr), b - (int) (yper * yincr));
    return (0);
}

do_move(buff)
char *buff;
{
    float xper, yper;

    if (sscanf(buff, "%*s %f %f", &xper, &yper) != 2)
        return (-1);
    if (xper < 0.0  || yper < 0.0  || xper > 100.0  || yper > 100.0)
        return (-1);

    Move_abs(l + (int)(xper * xincr), b - (int)(yper * yincr));
    return (0);
}


do_color(buff)
char *buff;
{
    char color[64];
    unsigned long colr;

    if (sscanf(buff, "%*s %s", color) != 1)
		return(-1);
    if ((int)(colr = XD_make_colr(dpy, win, scrn, colormap, color)) < 0)
		return(-1);
    XSetForeground(dpy, gc, colr);
    return (0);
}

char *do_poly(buff, infile, window_height)
char *buff;
FILE *infile;
int window_height;
{
    register i;
    int num;
    char origcmd[64];
    float xper, yper;
    char *malloc();
    char *fgets();
    char *to_return;

    sscanf(buff, "%s", origcmd);

    num = 0;

    for (;;) {
        if ((to_return = fgets(buff, 128, infile)) == NULL)
            break;

        if (!sscanf(buff, "%f %f", &xper, &yper))
            break;

        if (xper < 0.0 || yper < 0.0 || xper > 100.0 || yper > 100.0)
            break;
        check_alloc(num + 1);
        xarray[num] = (int) (xper * xincr);
        yarray[num] = window_height - (int) (yper * yincr);
        num++;
    }

    points = (XPoint *) malloc(num * sizeof(XPoint));

    for (i = 0; i < num; i++) {
        points[i].x = (short) xarray[i];
        points[i].y = (short) yarray[i];
    }


    if (num) {
        if (!strcmp(origcmd, "polygon"))
            XFillPolygon(dpy, win, gc, points, num,
                Complex, CoordModeOrigin);
        else
            XDrawLines(dpy, win, gc, points, num, CoordModeOrigin);
    }
    return (to_return);
}

do_size(buff)
char *buff;
{
    float xper, yper;

    if (2 != sscanf(buff, "%*s %f %f", &xper, &yper))
        return (-1);
    if (xper < 0.
        || yper < 0.
        || xper > 100.
        || yper > 100.)
        return (-1);

    Text_size((int) (xper * xincr), (int) (yper * yincr));
    return (0);
}

do_text(buff)
char *buff;
{
    char *ptr;

/* remove new line */
    for (ptr = buff; *ptr != 012; ptr++);
    *ptr = 0;

    ptr = buff;
    for (; *ptr != ' '; ptr++);
    for (; *ptr == ' '; ptr++);
    Text(ptr);
}

check_alloc(num)
int num;
{
    int to_alloc;

    if (num < coors_allocated)
        return;

    to_alloc = coors_allocated;
    while (num >= to_alloc)
        to_alloc += CHUNK;

    if (coors_allocated == 0) {
        xarray = (int *)falloc(to_alloc, sizeof(int));
        yarray = (int *)falloc(to_alloc, sizeof(int));
    } else {
        xarray = (int *) frealloc(
            (char *) xarray,
            to_alloc,
            sizeof(int),
            coors_allocated);
        yarray = (int *) frealloc(
            (char *) yarray,
            to_alloc,
            sizeof(int),
            coors_allocated);
    }

    coors_allocated = to_alloc;
}

do_icon(buff, window_height)
char *buff;
int window_height;
{
    double xper, yper;
    char type;
    int size;
    int ix, iy;

    if (4 != sscanf(buff, "%*s %c %d %lf %lf", &type, &size, &xper, &yper))
        return (-1);

    if (xper < 0.  || yper < 0.  || xper > 100.  || yper > 100.)
        return (-1);

    ix = (int) (xper * xincr);
    iy = window_height - (int) (yper * yincr);

    switch (type & 0177) {
    case 'o':

        XDrawLine(dpy, win, gc,
            ix - size, iy - size, ix - size, iy + size);

        XDrawLine(dpy, win, gc,
            ix - size, iy + size, ix + size, iy + size);

        XDrawLine(dpy, win, gc,
            ix + size, iy + size, ix + size, iy - size);

        XDrawLine(dpy, win, gc,
            ix + size, iy - size, ix - size, iy - size);

        break;

    case 'x':

        XDrawLine(dpy, win, gc,
            ix - size, iy - size, ix + size, iy + size);
        XDrawLine(dpy, win, gc,
            ix - size, iy + size, ix + size, iy - size);
        break;

    case '+':
    default:

        XDrawLine(dpy, win, gc,
            ix, iy - size, ix, iy + size);
        XDrawLine(dpy, win, gc,
            ix - size, iy, ix + size, iy);

        break;
    }
    return (0);
}
