/*  disp_args.c */
#ifndef         INCLUDE
#define         INCLUDE

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#endif

#include <stdio.h>
#include <X11/Shell.h>
#include "gis.h"

struct Cell_head window;
int nrows, ncols;

Arg win_args[10];
Arg shell_args[10];

extern Widget top_level;
int screen;
Display *display;

set_display_args()
{
    int i;
    int disp_w, disp_h, min_pix_r, min_pix_c;
    double map_aspect;

    printf("\n enter set_args");
    G_gisinit("Xgraph");
    if (G_get_window(&window) < 0) {
        char buf[200];

        sprintf(buf, "can't read current window parameters");
        G_fatal_error(buf);
        exit(1);
    }
    printf("\n after get window");

    nrows = G_window_rows();
    ncols = G_window_cols();
    printf("\n nrows = %d, ncols = %d", nrows, ncols);

    i = 0;
    XtSetArg(win_args[i], XtNwidth, (2 * ncols));
    i++;
    XtSetArg(win_args[i], XtNheight, (2 * nrows));
    i++;
    XtSetArg(win_args[i], XtNborderWidth, 5);

    display = XtDisplay(top_level);
    screen = DefaultScreen(display);

    disp_w = XDisplayWidth(display, screen);
    disp_h = XDisplayHeight(display, screen);

    printf("\n yucka");
    map_aspect = ((double) ncols) / ((double) nrows);

    if (nrows <= disp_h && ncols <= disp_w) {
        min_pix_r = nrows;
        min_pix_c = ncols;
    } else {
        min_pix_r = disp_h / 3.0;

        while (1) {
            min_pix_c = map_aspect * min_pix_r;

            if (min_pix_c <= disp_w)
                break;
            else
                min_pix_r *= 0.9;
        }
    }

    i = 0;
    XtSetArg(shell_args[i], XtNmaxWidth, disp_w);
    i++;
    XtSetArg(shell_args[i], XtNmaxHeight, disp_h);
    i++;
    XtSetArg(shell_args[i], XtNminWidth, min_pix_c);
    i++;
    XtSetArg(shell_args[i], XtNminHeight, min_pix_r);
    i++;
    XtSetArg(shell_args[i], XtNwidthInc, min_pix_c);
    i++;
    XtSetArg(shell_args[i], XtNheightInc, min_pix_r);
    i++;
    XtSetArg(shell_args[i], XtNminAspectX, min_pix_c);
    i++;
    XtSetArg(shell_args[i], XtNminAspectY, min_pix_r);
    i++;
    XtSetArg(shell_args[i], XtNmaxAspectX, min_pix_c);
    i++;
    XtSetArg(shell_args[i], XtNmaxAspectY, min_pix_r);
    i++;

    printf("\n yuck");
}
