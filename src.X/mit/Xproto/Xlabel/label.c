/*  %W%  %G%  */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include "options.h"
#include "gis.h"

Display *dpy;
Window win;
GC  gc;
XGCValues gc_values;
unsigned long gc_valuemask;
int scrn;
XWindowAttributes xwa;
XEvent report;
XImage *xi;

label()
{
    char achar[2];
    int SCREEN_X, SCREEN_Y;
    char answer[64];
    int T, B, L, R;
    int t, b, l, r;
    char *panel;
    int tsize;
    unsigned int window_width, window_height, border_width, depth;
    int x, y;
    Window root_return;

    /* Set the display to be the default display */
    if (!(dpy = XOpenDisplay(NULL))) {
        fprintf(stderr, " can't open display\n");
        exit(-1);
    }
    scrn = DefaultScreen(dpy);
    win = XD_get_cur_window(dpy, scrn);

    XGetGeometry(dpy, win, &root_return, &x, &y, &window_width,
        &window_height, &border_width, &depth);

    XGetWindowAttributes(dpy, win, &xwa);
    XSelectInput(dpy, win, ButtonPressMask);

    gc_values.line_width = 1;

    gc = XCreateGC(dpy, win, GCLineWidth, &gc_values);

    XGetGeometry(dpy, win, &root_return, &x, &y, &window_width,
        &window_height, &border_width, &depth);

    T = 0;
    B = window_height;
    L = 0;
    R = window_width;

    dots_per_line = (int) (size / 100.0 *
        (float) window_height);
    tsize = (int) (.8 * (float) dots_per_line);
    Text_size(tsize, tsize);

    achar[1] = 000;
    panel = G_tempfile();

    while (1) {
        G_clear_screen();
        printf("Move pointer to label location\n");
        printf(" BUTTON  MEANS\n");
        printf("  Left    Place label here\n");
        printf("  Right   No more labels\n");

        XNextEvent(dpy, &report);

        switch (((XButtonPressedEvent *)&report)->button) {
        case 1:
            SCREEN_X = ((XButtonPressedEvent *) & report)->x;
            SCREEN_Y = ((XButtonPressedEvent *) & report)->y;

            printf("\nNow type in label >  ");
            gets(answer);

            /* Clear out area for text */
            Move_abs(SCREEN_X, SCREEN_Y);
            Get_text_box(answer, &t, &b, &l, &r);
            t = t - 2;
            if (t < T)
                t = T;
            b = b + 2;
            if (b > B)
                b = B;
            l = l - 2;
            if (l < L)
                l = L;
            r = r + 2;
            if (r > R)
                r = R;
            /* Save the panel */
            xi = XGetImage(dpy, win, l + 1, t + 1, r - l - 1, b - t - 1,
                AllPlanes, XYPixmap);

            XSetForeground(dpy, gc, XD_make_colr(dpy, win, scrn,
                    xwa.colormap, backcolor));

            XFillRectangle(dpy, win, gc,
                l + 1, t + 1, r - l - 1, b - t - 1);


            /* Draw text */
            Move_abs(SCREEN_X, SCREEN_Y);
            XSetForeground(dpy, gc, XD_make_colr(dpy, win, scrn,
                    xwa.colormap, textcolor));

            Text(answer);
            XFlush(dpy);

            if (is_not_ok()) {
                /* R_panel_restore(panel) ; */
                XPutImage(dpy, win, gc,
                    xi, 0, 0, l + 1, t + 1, r - l - 1, b - t - 1);
            } else {
                SCREEN_Y = b + dots_per_line;
                if (SCREEN_Y > B)
                    SCREEN_Y = B;
            }
            break;
        case 2:
            break;

        case 3:
            XDestroyImage(xi);
            return (0);
        }
    }
}

static is_not_ok()
{
    char answer[64];

    for (;;) {
        printf("\nIs this OK?  y/n : ");
        gets(answer);
        switch (answer[0] & 0177) {
        case 'y':
        case 'Y':
            return (0);
        case 'n':
        case 'N':
            return (1);
        default:
            break;
        }
    }
}

