#include <stdio.h>
#include <signal.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include "sun.h"

#define NAMEW	"GRASS3b"
#define BORDER	1
#define FONT "8x13"

/* This program is a rewrite of the original Graph_Set from the GRASS
 * 3.0 version. All suncore and sunview related stuff (which was
 * the bulk of the original code) has been replaced by X11 library
 * calls. All non-suncore code has been retained. Note that this code
 * creates an X11 window but does not map it (make it visible). That
 * task is left up to the calling routine SWITCHER.
 */

int SCREEN_LEFT;
int SCREEN_RIGHT;
int SCREEN_BOTTOM;
int SCREEN_TOP;
int SC_WID;
int SC_HITE;
int NCOLORS = 256;

/* declare some variables that will be externs in some other routines */
Display *dpy;
Window  grwin;
Pixmap  bkupmap;
GC      gc;
Colormap grasscmap;
Cursor  grcurse,grxh;
unsigned long gemask;
char *grasstr = "GRASS-X Display Window";
#define MAIN

/*
 * This structure forms the WM_HINTS property of the window,
 * letting the window manager know how to handle this window.
 */
XWMHints	xwmh = {
    (InputHint|StateHint),	/* flags */
    False,			/* input */
    NormalState,		/* initial_state */
    0,				/* icon pixmap */
    0,				/* icon window */
    0, 0,			/* icon location */
    0,				/* icon mask */
    0,				/* Window group */
};
    
XFontStruct *fontstruct;	/* Font descpritor */

int Graph_Set()
{
    
    unsigned long fth;		/* Font size parameter */
    unsigned long fg, bg, bd;	/* Pixel values */
    unsigned long bw;		/* Border width */
    XGCValues   gcv;		/* Struct for creating GC */
    XSizeHints  xsh;		/* Size hints for window manager */
    char       *geomSpec;	/* Window geometry string */
    XSetWindowAttributes xswa;	/* Temporary Set Window Attribute struct */
    XWindowAttributes xwa;	/* Temporary Get Window Attribute struct */
    XColor	sd;		/* Temp set colormap entry struct */

    int sigint();
    int x,y,i;

    /*
     * Open the display using the $DISPLAY environment variable to locate
     * the X server. Return 0 if cannot open. 
     */
    if ((dpy = XOpenDisplay(NULL)) == NULL) {
	fprintf(stderr, "Graph_Set: can't open %s\n", XDisplayName(NULL));
	fflush(stderr);
	exit(-1);
    }

    /*
     * Load the font to use. GRASS doesn't at this point use fonts,
     * but may someday in the future.
     */
    if ((fontstruct = XLoadQueryFont(dpy, FONT)) == NULL) {
	fprintf(stderr, "Graph_SetX: display %s doesn't know font %s\n",
		DisplayString(dpy), FONT);
	fflush(stderr);
	exit(-1);
    }
    fth = fontstruct->max_bounds.ascent + fontstruct->max_bounds.descent;

    /*
     * Select colors for the border,  the window background,  and the
     * foreground.
     */
    bd = WhitePixel(dpy, DefaultScreen(dpy));
    bg = BlackPixel(dpy, DefaultScreen(dpy));
    fg = WhitePixel(dpy, DefaultScreen(dpy));

    /*
     * Set the border width of the window,
     */
    bw = 1;

    /*
     * Deal with providing the window with an initial position & size.
     * Use values close to CEDR's GRASS2 implementation
     */
    xsh.flags = (PPosition | PSize);
    xsh.height = 400;
    xsh.width = 400;
    xsh.x = 0; xsh.y = 0;

    /*
     * Create the Window with the information in the XSizeHints, the
     * border width,  and the border & background pixels. 
     */
    grwin = XCreateSimpleWindow(dpy, DefaultRootWindow(dpy),
			      xsh.x, xsh.y, xsh.width, xsh.height,
			      bw, bd, bg);

    /*
     * Set the standard properties for the window managers.
     */
    XSetStandardProperties(dpy, grwin, NAMEW, NAMEW, None, &grasstr, 1, &xsh);
    XSetWMHints(dpy, grwin, &xwmh);

    /* Create a colormap for the grass window, so that colormap
     * changes do not alter the original default colormap. Set alternate
     * entries in this colormap to white, black respectively.
     * 1st we need to get the window attributes so we can get the
     * pointer to the window's visual structure.
     */
    if (XGetWindowAttributes(dpy,grwin,&xwa) == 0) {
	fprintf(stderr,"Can't get the window attributes\n");
	exit (-1);
    }
    grasscmap = XCreateColormap(dpy,grwin,xwa.visual,AllocAll);
    sd.flags = ( DoRed | DoGreen | DoBlue );
    for (i=0; i<256; i++){
	sd.pixel = i;
	if (i%2 == 0)
    	    sd.red = sd.green = sd.blue = 0;
	else
    	   sd.red = sd.green = sd.blue = 65535;
        XStoreColor(dpy,grasscmap,&sd);
    }


    /*
     * Create the diamond hand and dot cursors to be used later
     */
     grcurse = XCreateFontCursor(dpy,XC_diamond_cross);
     grxh = XCreateFontCursor(dpy,XC_crosshair);

    /*
     * Start with the window's colormap field pointing to the default
     * colormap. Later pointer enter events will cause a switch to 
     * the grass color map. Also, set Bit Gravity to reduce Expose events.
     */
    xswa.colormap = grasscmap;
/*  xswa.colormap = DefaultColormap(dpy, DefaultScreen(dpy)); */
    xswa.bit_gravity = CenterGravity;
    xswa.backing_store = Always;
    XChangeWindowAttributes(dpy, grwin, 
	  (CWColormap | CWBitGravity | CWBackingStore ), &xswa);

    /*
     * Create the GC for writing the text.
     */
    gcv.font = fontstruct->fid;
    gcv.foreground = fg;
    gcv.background = bg;
    gc = XCreateGC(dpy, grwin, (GCFont | GCForeground | GCBackground), &gcv);

    /*
     * Specify only expose events wanted at this point. We want to
     * get the window visible without any interference from pointer
     * motion, ButtonPress, Enter/Exit window events.
     */
    XSelectInput(dpy, grwin, ExposureMask);

    /*
     * Map the window to make it visible. This causes an expose event
     */
    XMapWindow(dpy,grwin);
    XFlush(dpy);
    while(1) 
	{
	XGetWindowAttributes(dpy,grwin,&xwa);
	if (xwa.map_state == IsViewable)
	    break;
	}
    Service_Xevent(0);

    /*
     * Now Specify ALL the event types we're interested in - Exposures,
     * ButtonPress, pointer motion, Enter/Exit window wanted.
     */
    gemask = (	ExposureMask | ButtonPressMask |
		EnterWindowMask | LeaveWindowMask );
    XSelectInput(dpy, grwin, gemask);

    /*
     * Find out how big the window really is (in case window manager
     * overrides our request) and set the SCREEN values.
     */
    SCREEN_LEFT = 0;
    SCREEN_TOP = 0;
    if (XGetWindowAttributes(dpy,grwin,&xwa) == 0) {
	fprintf(stderr,"Graph_Set: cannot get window attributes\n");
	exit(-1);
	}
    SCREEN_RIGHT = xwa.width-1;
    SCREEN_BOTTOM = xwa.height-1;
    fprintf(stderr,"S_R=%d, S_B=%d S_L=%d S_T=%d\n", 
	    SCREEN_RIGHT, SCREEN_BOTTOM, SCREEN_LEFT, SCREEN_TOP);
    SC_WID = xwa.width; SC_HITE = xwa.height;

    /*
     * Now create a pixmap that will contain same contents as the 
     * window. It will be used to redraw from after expose events
     */
    bkupmap = XCreatePixmap(dpy,grwin,SC_WID,SC_HITE,8);
    
    /*
     * prepare to catch signals 
     */
    signal (SIGHUP, sigint);
    signal (SIGINT, sigint);
    signal (SIGQUIT, sigint);
    signal (SIGILL, sigint);
    signal (SIGTSTP, SIG_IGN);

    /*  note that x,y not yet known */
    sun_x = sun_y = 999999999;

    /* all done, */
}

static
sigint()
{
    Graph_Close();
    exit(-1);
}
