/*
 *   xchoose
 *   Usage:  Xchoose name
 *           Xchoose     (choose with mouse if on tty)
 *   Choose a window on the screen
 */

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>

#define USAGE1 "name"
#define USAGE2 "   (to use mouse)"

Display *dpy;
int scrn;
char cur_wind[32];

main(argc, argv)
char *argv[];
{
    int dummyi;
    unsigned dummy;
    Window XD_get_cur_window();
    Window root_window, win, prev_window;
    Window Select_Window(), Select_Window_Args();
    Atom property;

    if (argc > 2) {
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE1);
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE2);
        exit(1);
    }
    /* Set the display to be the default display */
    if (!(dpy = XOpenDisplay(NULL))) {
        fprintf(stderr, " can't open display\n");
        exit(-1);
    }
    scrn = DefaultScreen(dpy);
    root_window = DefaultRootWindow(dpy);
    prev_window = XD_get_cur_window(dpy, scrn);
    fprintf(stderr, "previous window = 0x%lx\n", prev_window);

    win = Select_Window_Args(&argc, argv);
    if (!win)
        /* select a window using the mouse */
        win = Select_Window(dpy, scrn);
    if ((win != root_window) && (win != prev_window) &&
            XGetGeometry(dpy, win, &root_window, &dummyi, &dummyi,
            &dummy, &dummy, &dummy, &dummy))
        win = XmuClientWindow(dpy, win);

    fprintf(stderr, "selected window = 0x%lx\n", win);
    (void)sprintf(cur_wind, "0x%lx", win);

    if ((property = XInternAtom(dpy, "GRASS_WINDOW", True)) == None) {
        fprintf(stderr,
            "argv[0]: no \"GRASS_WINDOW\"window atom exists.\n");
        exit(0);
    }
    XChangeProperty(dpy, root_window, property, XA_STRING, 8,
        PropModeReplace, (u_char*)cur_wind, strlen(cur_wind));

    if (prev_window != win) {
        XSetWindowBorder(dpy, win, WhitePixel(dpy, scrn));
        if (prev_window != NULL)
            XSetWindowBorder(dpy, prev_window, BlackPixel(dpy, scrn));
    }
    XFlush(dpy);
}


/* The following was liberally rip out of xwininfo from X11r4 */
/* Copyright 1987, Massachusetts Institute of Technology */
/* Author: Mark Lillibridge, MIT Project Athena, 16-Jun-87 */

/* Select_Window_Args: a rountine to provide a common interface for
 *            applications that need to allow the user to select one
 *            window on the screen for special consideration.
 *            This routine implements the following command line
 *            arguments:
 *
 *            -root            Selects the root window.
 *            -id <id>         Selects window with id <id>. <id> may
 *                             be either in decimal or hex.
 *            -name <name>     Selects the window with name <name>.
 *
 *            Call as Select_Window_Args(&argc, argv) in main before
 *            parsing any of your program's command line arguments.
 *            Select_Window_Args will remove its arguments so that
 *            your program does not have to worry about them.
 *            The window returned is the window selected or 0 if
 *            none of the above arguments was present.  If 0 is
 *            returned, Select_Window should probably be called after
 *            all command line arguments, and other setup is done.
 *            For examples of usage, see xwininfo, xwd, or xprop.
 */
#define ARGC (*rargc)
#define OPTION argv[0]
#define NXTOPTP ++argv, --argc>0
#define NXTOPT if (++argv, --argc==0) exit()
#define COPYOPT nargv++[0]=OPTION; nargc++

Window Select_Window_Args(rargc, argv)
int *rargc;
char **argv;
{
    int nargc = 1;
    int argc;
    char **nargv;
    Window w = 0, Window_With_Name();

    nargv = argv + 1;
    argc = ARGC;
    while (NXTOPTP) {
        if (!strcmp(OPTION, "-")) {
            COPYOPT;
            while (NXTOPTP)
                COPYOPT;
            break;
        }
        if (!strcmp(OPTION, "-root")) {
            w = RootWindow(dpy, scrn);
            continue;
        }
        if (!strcmp(OPTION, "-name")) {
            NXTOPT;
            w = Window_With_Name(dpy, RootWindow(dpy, scrn), OPTION);
            if (!w) {
                fprintf(stderr,"No window with name %s exists!",OPTION);
                exit(-1);
            }
            continue;
        }
        if (!strcmp(OPTION, "-id")) {
            NXTOPT;
            w = 0;
            sscanf(OPTION, "0x%lx", &w);
            if (!w)
                sscanf(OPTION, "%ld", &w);
            if (!w) {
                fprintf(stderr, "Invalid window id format: %s.",OPTION);
                exit(-1);
            }
            continue;
        }
        COPYOPT;
    }
    ARGC = nargc;
    return(w);
}


/*
 * Routine to let user select a window using the mouse
 */
Window Select_Window(dpy)
Display *dpy;
{
    int status;
    Cursor cursor;
    XEvent event;
    Window target_win = None, root = DefaultRootWindow(dpy);
    int buttons = 0;

    /* Make the target cursor */
    cursor = XCreateFontCursor(dpy, XC_crosshair);

    /* Grab the pointer using target cursor, letting it room all over */
    status = XGrabPointer(dpy, root, False,
        ButtonPressMask | ButtonReleaseMask, GrabModeSync,
        GrabModeAsync, root, cursor, CurrentTime);
    if (status != GrabSuccess)
        fprintf(stderr, "Can't grab the mouse.");

    /* Let the user select a window... */
    while ((target_win == None) || (buttons != 0)) {
        /* allow one more event */
        XAllowEvents(dpy, SyncPointer, CurrentTime);
        XWindowEvent(dpy, root, ButtonPressMask | ButtonReleaseMask,
                &event);
        switch (event.type) {
        case ButtonPress:
            if (target_win == None) {
                target_win = event.xbutton.subwindow;   /* window selected */
                if (target_win == None)
                    target_win = root;
            }
            buttons++;
            break;
        case ButtonRelease:
            if (buttons > 0)    /* there may have been some down */
                buttons--;      /* before we started */
            break;
        }
    }
    XUngrabPointer(dpy, CurrentTime);   /* Done with pointer */
    return(target_win);
}


/*
 * Window_With_Name: routine to locate a window with a given name on a
 *      display.  If no window with the given name is found, 0 is
 *      returned. If more than one window has the given name, the
 *      first one found will be returned.  Only top and its subwindows
 *      are looked at.  Normally, top should be the RootWindow.
 */
Window Window_With_Name(dpy, top, name)
Display *dpy;
Window top;
char *name;
{
    Window *children, dummy;
    unsigned int nchildren;
    int i;
    Window w = 0;
    char *window_name;

    if (XFetchName(dpy, top, &window_name) && !strcmp(window_name,name))
        return (top);
    if (!XQueryTree(dpy, top, &dummy, &dummy, &children, &nchildren))
        return (0);
    for (i = 0; i < nchildren; i++) {
        w = Window_With_Name(dpy, children[i], name);
        if (w)
            break;
    }
    if (children)
        XFree((char *)children);
    return (w);
}

/*** end xchoose.c ***/
