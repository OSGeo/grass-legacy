#include <stdio.h>
#include <signal.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include "icon.bit"

Display *dpy;
int scrn;
XEvent xevent;
char *progName;
static char window_id[32];
static char buf[100];

#define START_X 20
#define START_Y 10
#define BORDER_WIDTH    3
#define MIN_WIDTH       550
#define MIN_HEIGHT      450


main(argc, argv)
int argc;
char **argv;
{
    Atom cell, overlay, font, colormode, vect, property;
    XSetWindowAttributes xswa;
    char *display_name = NULL, *winGeom = NULL;
    int winWidth, winHeight;
    Colormap make_fixed_colormap(), load_vect_colrs();
    Colormap make_float_clr_table();
    int i, ncolors, Quit();
    Window p_win, win;
    u_long nitems, remaining;
    u_char *one, *two, *three;
    int actual_format;
    Atom actual_type, closedownAtom, wmProtocolsAtom;
    XTextProperty windowName, iconName;
    XSizeHints *szhints;
    XClassHint *clshints;
    XWMHints  *wmhints;
	Colormap cmap;
	static char vect_name[32], cell_name[32], overlay_name[32];
	static char font_name[32], color_mode[32];

    progName = argv[0];
    G_gisinit(progName);
    /* connection to X server */
    if (!(dpy = XOpenDisplay(display_name))) {
        fprintf(stderr, "Xnew : cannot connect to X server %s\n",
            XDisplayName(display_name));
        exit(-1);
    }
    scrn = DefaultScreen(dpy);
    /* sizing of window */
    winWidth = DisplayWidth(dpy, scrn) / 2.5;
    winHeight = DisplayHeight(dpy, scrn) / 2.5;
    ncolors = DisplayCells(dpy, scrn);
    cmap = XCreateColormap(dpy, DefaultRootWindow(dpy),
            DefaultVisual(dpy, scrn), AllocAll);
    cmap = make_fixed_colormap(cmap, ncolors);

    /* initialization of size hint property for wm */
    if (!winGeom)
        winGeom = XGetDefault(dpy, progName, "geometry");
    szhints = XAllocSizeHints();
    SetSizeHints(szhints, winGeom, winWidth, winHeight, BORDER_WIDTH);
    xswa.colormap = cmap;
    xswa.event_mask = ExposureMask | KeyPressMask | PropertyChangeMask;
    xswa.background_pixel = BlackPixel(dpy, scrn);
    xswa.border_pixel = WhitePixel(dpy, scrn);
    xswa.backing_store = Always;
    xswa.save_under = True;
    win = XCreateWindow(dpy, DefaultRootWindow(dpy), szhints->x,
        szhints->y, (unsigned)szhints->width, (unsigned)szhints->height,
        BORDER_WIDTH, DefaultDepth(dpy, scrn), InputOutput,
        DefaultVisual(dpy, scrn), (CWBackPixel | CWColormap |
        CWEventMask | CWBackingStore | CWSaveUnder | CWBorderPixel),
        &xswa);

    XMapWindow(dpy, win);

    wmhints = XAllocWMHints();
    /* properties for window manager */
    wmhints->icon_pixmap = XCreateBitmapFromData(dpy, win, icon_bits,
            icon_width, icon_height);
    wmhints->flags |= IconPixmapHint;
        iconName.encoding = XA_STRING;
    iconName.format = 8;
    iconName.value = (u_char *)progName;
    iconName.nitems = strlen((char*)iconName.value);
    windowName.encoding = iconName.encoding = XA_STRING;
    windowName.format = iconName.format = 8;
    windowName.value = (u_char *)progName;
    windowName.nitems = strlen((char*)windowName.value);
    clshints = XAllocClassHint();
    clshints->res_name = NULL;
    clshints->res_class = "Xxim";
    XSetWMProperties(dpy, win, &windowName, &iconName, argv, argc,
            szhints, wmhints, clshints);
    closedownAtom = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
    wmProtocolsAtom = XInternAtom(dpy, "WM_PROTOCOLS", False);
    (void)XSetWMProtocols(dpy, win, &closedownAtom, 1);

    (void)signal(SIGHUP,  Quit);
    (void)signal(SIGINT,  Quit);
    (void)signal(SIGQUIT, Quit);
    if ((property = XInternAtom(dpy, "GRASS_WINDOW", True)) == None)
        fprintf(stderr, "no \"GRASS_WINDOW\" atom exists\n");
    (void)sprintf(window_id, "0x%lx", win);
    XChangeProperty(dpy, DefaultRootWindow(dpy), property, XA_STRING,
            8, PropModeReplace, (u_char*)window_id, strlen(window_id));
    printf("window id: 0x%lx\n", win);
    (void)fflush(stdout);

    /* set properties */
    strcpy(cell_name, "None");
    strcpy(vect_name, "None");
    strcpy(overlay_name, "None");
    strcpy(font_name, "None");
    strcpy(color_mode, "fixed");

    cell = XInternAtom(dpy, "grass_cell", False);
    vect = XInternAtom(dpy, "grass_vect", False);
    overlay = XInternAtom(dpy, "grass_overlay", False);
    font = XInternAtom(dpy, "grass_font", False);
    colormode = XInternAtom(dpy, "grass_colormode", False);

    XChangeProperty(dpy, win, cell, XA_STRING, 8,
            PropModeReplace, (u_char*)cell_name, strlen(cell_name));
    XChangeProperty(dpy, win, vect, XA_STRING, 8,
            PropModeReplace, (u_char*)vect_name, strlen(vect_name));
    XChangeProperty(dpy, win, overlay, XA_STRING, 8, PropModeReplace,
			(u_char*)overlay_name, strlen(overlay_name));
    XChangeProperty(dpy, win, font, XA_STRING, 8,
            PropModeReplace, (u_char*)font_name, strlen(font_name));
    XChangeProperty(dpy, win, colormode, XA_STRING, 8,
            PropModeReplace, (u_char*)color_mode, strlen(color_mode));

    /* event handling */
    while (True) {
        XNextEvent(dpy, &xevent);
        switch((int)xevent.type) {
        case Expose:
            while (XCheckTypedEvent(dpy, Expose, &xevent))
                ;
            p_win = XD_get_cur_window(dpy, scrn);
            if (p_win != win)
                break;

            XGetWindowProperty(dpy, win, cell, 0L, 320L, False,
                    XA_STRING, &actual_type, &actual_format, &nitems,
                    &remaining, &one);
            if (sscanf(one, "%s", cell_name) != 1)
                break;
            if (strcmp("None", cell_name)) {
                (void)sprintf(buf, "Xcell %s", cell_name);
                fprintf(stderr, " %s\n", buf);
                system(buf);
            }
            XGetWindowProperty(dpy, win, overlay, 0L, 320L, False,
                    XA_STRING, &actual_type, &actual_format, &nitems,
                    &remaining, &two);
            if (sscanf(two, "%s", overlay_name) != 1)
                break;
            if (strcmp("None", overlay_name)) {
                (void)sprintf(buf, "Xoverlay %s", overlay_name);
                fprintf(stderr, " %s\n", buf);
                system(buf);
            }
            XGetWindowProperty(dpy, win, vect, 0L, 320L, False,
                XA_STRING, &actual_type, &actual_format, &nitems,
                &remaining, &three);
            if (sscanf(three, "%s", vect_name) != 1)
                break;
            if (strcmp("None", vect_name)) {
                (void)sprintf(buf, "Xvect %s white", vect_name);
                fprintf(stderr, " %s\n", buf);
                system(buf);
            }
            break;
        case KeyPress: {
                char c;
                i = XLookupString(&xevent.xkey, &c, 1, (KeySym*)NULL,
                        (XComposeStatus*)NULL);
                if ((i == 1) &&
                        ((c == 'q') || (c == 'Q') || (c == '\003')))
                    Quit();
            }
            break;
        case PropertyNotify:
            /* fprintf(stderr, " property changed\n"); */
            break;
        case ClientMessage: {
            XClientMessageEvent *xmessage;

            if ((xevent.xclient.message_type == wmProtocolsAtom) &&
                    (xevent.xclient.data.l[0] == closedownAtom))
                Quit();
            xmessage = (XClientMessageEvent *)&xevent;
            fprintf(stderr, "message = %s\n", (char*)xmessage->data.b);
            if (strcmp("fixed", (char *)(xmessage->data.b)) == 0)
                cmap = make_fixed_colormap(cmap, ncolors);
            else if (strcmp("float", (char*)xmessage->data.b) == 0)
                cmap = load_vect_colrs(cmap);
            else
                cmap = make_float_clr_table(cmap, xmessage->data.b);
            break;
        }
        default:
            fprintf(stderr, "%s: Bad event received\n", progName);
            break;
        }
    }
}

int Quit()
{
    Atom property;

    if ((property = XInternAtom(dpy, "GRASS_WINDOW", True)) == None) {
        fprintf(stderr, "Xnew: no window atom exists.\n");
        return(0);
    }
    (void)sprintf(window_id, "0x%lx", 0);
    XChangeProperty(dpy, DefaultRootWindow(dpy), property, XA_STRING,
            8, PropModeReplace, (u_char*)window_id, strlen(window_id));
    XFlush(dpy);
    fprintf(stderr, "\n%s: quitting.\n", progName);
    exit(0);
}

/* set the window size hints
*/
SetSizeHints(shints, geom, width, height, bdwidth)
XSizeHints *shints;
char *geom;
unsigned width, height, bdwidth;
{
    int geombits = NoValue;

    shints->width = width;
    shints->height = height;
    shints->x = shints->y = 10;
    shints->flags = (PSize | PPosition);
    geombits = XWMGeometry(dpy, scrn, geom, (char*)0, bdwidth, shints,
            &shints->x, &shints->y, &shints->width, &shints->height,
            &shints->win_gravity);
    shints->flags |= PWinGravity;
    if (geombits & (WidthValue|HeightValue)) {
        shints->width = shints->width > shints->min_width ?
            shints->width : shints->min_width;
        shints->height = shints->height > shints->min_height ?
            shints->height : shints->min_height;
        shints->flags |= USSize;
        shints->flags &= ~PSize;
    } else {
        shints->width = width;
        shints->height = height;
        shints->flags |= PSize;
    }
    if (geombits & (XValue|YValue)) {
        shints->flags |= USPosition;
        shints->flags &= ~PPosition;
    }
    if (!(shints->flags & USPosition))
        shints->flags |= PPosition;
}

/*** xnew.c ***/
