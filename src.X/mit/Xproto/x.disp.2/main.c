#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Shell.h>
#include "gis.h"
#include "icon.bit"

Widget top_level;
extern Widget display, coors;
Window display_win, coor_win;
Display *dpy;
int scrn;
GC  coor_gc;
unsigned disp_width, disp_height;
char cell_displayed[25], *vector_files[5];
int n_vects = 0;
double xl, xr, yt, yb;

main(argc, argv)
int argc;
char *argv[];
{
    extern void create_display(), attach_window_info();
    XSetWindowAttributes xswa;
    register i;
    Drawable drawable_root;
    int x, y;
	unsigned border_width, drawable_depth;
    extern struct Cell_head window;
    XGCValues gcval;
	char window_id[32];
    Atom  property;
	Colormap cmap;
	Arg  arglist[10];
	Pixmap disp_icon;
	unsigned int disp_w, disp_h;

    /* X toolkit initialization and creation of the toplevel widget */
    top_level = XtInitialize("XGrass", "XGrass", NULL, 0, &argc, argv);
    dpy = XtDisplay(top_level);
    scrn = DefaultScreen(dpy);

    cmap = XCreateColormap(dpy, DefaultRootWindow(dpy),
		DefaultVisual(dpy, scrn), AllocAll);
	cmap = make_fixed_colormap(cmap, DisplayCells(dpy, scrn));
    disp_icon = XCreatePixmapFromBitmapData(dpy, DefaultRootWindow(dpy),
        icon_bits, icon_width, icon_height, (unsigned long)pixel("red"),
        (unsigned long)pixel("yellow"), 1);
    disp_w = DisplayWidth(dpy, scrn);
    disp_h = DisplayHeight(dpy, scrn);
    XtSetArg(arglist[0], XtNminWidth, 600);
    XtSetArg(arglist[1], XtNminHeight, 500);
    XtSetArg(arglist[2], XtNheight, 500);
    XtSetArg(arglist[3], XtNwidth, 600);
    XtSetArg(arglist[4], XtNmaxWidth, disp_w);
    XtSetArg(arglist[5], XtNmaxHeight, disp_h);
    XtSetArg(arglist[6], XtNiconPixmap, disp_icon);
	XtSetArg(arglist[7], XtNcolormap, (XtArgVal)cmap);
    XtSetArg(arglist[8], XtNallowShellResize, True);
	XtSetValues(top_level, arglist, 9);

    create_display();
    XtRealizeWidget(top_level);
	XSetWindowColormap(dpy, XtWindow(top_level), cmap);

    display_win = XtWindow(display);
    coor_win = XtWindow(coors);
    attach_window_info();

    /* calc window bounds inside the display win     */
    XGetGeometry(dpy, display_win, &drawable_root, &x, &y, &disp_width,
        &disp_height, &border_width, &drawable_depth);
    XD_get_screen_bounds(&xl, &xr, &yt, &yb, window.north, window.south,
		window.west, window.east, disp_width, disp_height);

	fprintf(stderr, "Window id: 0x%lx\n", display_win);
    if ((property = XInternAtom(dpy, "GRASS_WINDOW", True)) == None)
        fprintf(stderr, "no \"GRASS_WINDOW\" atom exists\n");
    (void)sprintf(window_id, "0x%lx", display_win);
    XChangeProperty(dpy, DefaultRootWindow(dpy), property, XA_STRING,
        8, PropModeReplace, (u_char*)window_id, strlen(window_id));

    coor_gc = XCreateGC(dpy, coor_win, None, &gcval);
    XSetForeground(dpy, coor_gc, 1);
    xswa.backing_store = Always;
    XChangeWindowAttributes(dpy, display_win, CWBackingStore, &xswa);

    for (i = 0; i < 5; i++)
        vector_files[i] = NULL;

    XtMainLoop();
}
