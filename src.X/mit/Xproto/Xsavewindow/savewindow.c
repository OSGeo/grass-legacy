
#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#define PUT(x, nx)  fwrite((char *) x,nx,1,out)

Display *dpy;
Window win;
int scrn;
XWindowAttributes xwa;
Colormap cmap;

savescreen(out)
FILE *out;
{
    static char *werr = "Write Error";
    int NPLANES;
    int i, ncolors;
    XColor answer;
    XImage *image;
    float red, grn, blu;
    int cellsize;
    int nrows, ncols;
    u_long pixel_data;
    int row, col;

    /* Set the display to be the default display */
    if (!(dpy = XOpenDisplay(NULL))) {
        printf(" can't open display\n");
        return (-1);
    }
    scrn = DefaultScreen(dpy);
    win = DefaultRootWindow(dpy);

    if (!XGetWindowAttributes(dpy, win, &xwa)) {
        fprintf(stderr, "Can't get root window attributes\n");
        return(0);
    }

    cmap = xwa.colormap;
    NPLANES = xwa.depth;
    nrows = xwa.height;
    ncols = xwa.width;

    /* determine the number of color registers needed for NPLANES */
    /* ncolors = 2 raised to the NPLANES power          */

    for (ncolors = 1, i = 0; i < NPLANES; i++)
        ncolors *= 2;

    PUT(&ncolors, sizeof ncolors);

    /* write the color table: number of colors followed by color % */
    for (i=0; i < ncolors; i++) {
        answer.pixel = (u_long) i;
        XQueryColor(dpy, cmap, &answer);
        red = (float)answer.red / 65535.0;
        grn = (float)answer.green / 65535.0;
        blu = (float)answer.blue / 65535.0;

        PUT(&red, sizeof red);
        PUT(&grn, sizeof grn);
        PUT(&blu, sizeof blu);
    }
    cellsize = sizeof(u_long);
    PUT(&cellsize, sizeof cellsize);

    /* write number of rows and number of cols */
    PUT(&nrows, sizeof nrows);
    PUT(&ncols, sizeof ncols);

    /* Snarf the pixmap with XGetImage */
    image = XGetImage(dpy, win, 0, 0, ncols, nrows, AllPlanes, ZPixmap);

    if (!image) {
        fprintf(stderr, "\n  unable to get image.\n");
        exit(1);
    }
    /* Write out image data */
    for (row = 0; row < nrows; row++) {
        for (col = 0; col < ncols; col++) {
            pixel_data = XGetPixel(image, col, row);
            PUT(&pixel_data, sizeof pixel_data);
        }
    }
    return(1);
}

