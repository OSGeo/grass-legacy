#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "../lib/graph.h"
#include "colors.h"


extern int SCREEN_RIGHT, SCREEN_BOTTOM ;
extern XFontStruct *fontstruct;
extern Display *dpy;
extern Window grwin;
extern Pixmap bkupmap;
extern int backing_store;
extern GC gc;
extern unsigned SC_WID, SC_HITE;

typedef struct _list {
    char *value;
    struct _list *next;
} LIST;

typedef struct _item_ {
    char *name;
    LIST *list;
    struct _item_ *next, *prev;
} ITEM;

typedef struct _pad_ {
    char *name;
    ITEM *items;
    struct _pad_ *next, *prev;
} PAD;

PAD *find_pad();

extern PAD *curpad;
extern PAD *padlist;

static char grasstr[] = "GRASS 4.0 - X Display Window";


int Service_Xevent(wait)
int wait;
{
    static int firstime = 1;
    XEvent event;

    /* If wait is zero wait for and service the next X event. If wait
     * is non-zero see if any events are ready. If none just return. */
    if (wait) {
        if (!XPending(dpy)) /* this won't die if X server quits (shapiro) */
        {
            XSync(dpy, False); /* check if server still lives (shapiro) */
            return;             /* no events in queue, return */
        }
        XNextEvent(dpy, &event);
    }
    /* On the first Expose events, write the grass display message. For
     * now, on subsequent expose copy the backup window to the display
     * window. */
    if ((wait == 0) || (event.type == Expose && event.xexpose.count == 0)
        || event.type == ConfigureNotify ) {
      if ( event.type == Expose ) {
        if (firstime) {
            XWindowAttributes xwa;
            int x, y;

            firstime = 0;

            /* Get the window's current attributes. */
            if (XGetWindowAttributes(dpy, grwin, &xwa) == 0)
                return;
            x = (xwa.width -
                XTextWidth(fontstruct, grasstr, strlen(grasstr))) / 2;
            y = (xwa.height + fontstruct->max_bounds.ascent
                - fontstruct->max_bounds.descent) / 2;
            /* Fill the window with the background color,  and then
             * paint the centered string. */
            XClearWindow(dpy, grwin);
            XDrawString(dpy, grwin, gc, x, y, grasstr, strlen(grasstr));
        } else 
        {
            XWindowAttributes xwa;

            /* Get the window's current attributes. */
            if (XGetWindowAttributes(dpy, grwin, &xwa) == 0)
                return;

            SC_WID  = xwa.width;
            SC_HITE = xwa.height;
            SCREEN_RIGHT = xwa.width - 1;
            SCREEN_BOTTOM = xwa.height - 1;
            Set_window(1, xwa.height-1, 1, xwa.width-1);

            if (backing_store != Always)
                XCopyArea(dpy, bkupmap, grwin, gc, 0, 0, SC_WID, SC_HITE,
                    0, 0);
            firstime = 0;
        }
      } else if ( event.type == ConfigureNotify ) {
            /* if the window is not the same size do a d.frame -e */
            if ( event.xconfigure.width != SC_WID || 
                 event.xconfigure.height != SC_HITE ) {
                PAD *curpad;
                int t, b, l, r;
                char buf[64];
                XWindowAttributes xwa;


                /* Get the window's current attributes. */
                if (XGetWindowAttributes(dpy, grwin, &xwa) == 0)
                    return;

                SC_WID  = xwa.width;
                SC_HITE = xwa.height;
                SCREEN_RIGHT = xwa.width - 1;
                SCREEN_BOTTOM = xwa.height - 1;

                /* do a d.frame -e (essentially) */
                /* Dclearscreen() */
                /* delete the time and current window out of the scratch pad */
                curpad = find_pad("");
                delete_item(curpad,"time");
                delete_item(curpad,"cur_w");
                /* delete all other pads */
                for ( curpad = padlist; curpad != NULL; curpad = curpad->next ){
                    if ( *curpad->name  )
                        delete_pad(curpad);
                }
                curpad = NULL;
                /* set standard color to black and erase */
                Standard_color(BLACK);
                Erase();
                /* Dnew("full_screen") */
                /* find a pad called "full_screen" */
                create_pad("full_screen");
                sprintf(buf,"1 %d 1 %d",SCREEN_BOTTOM,SCREEN_RIGHT);
                curpad = find_pad("full_screen");
                append_item(curpad, "d_win", buf);
                _time_stamp(curpad);
                /* Dchoose("full_screen") */
                /* set the time and window name in no-name pad */
                curpad = find_pad("");
                append_item(curpad, "cur_w", "full_screen");
                _time_stamp(curpad);
                /* make white outline for the window (it's selected) */
                Standard_color(WHITE);
                Move_abs(0, SCREEN_BOTTOM+1) ;
                Cont_abs(0, 0) ;
                Cont_abs(SCREEN_RIGHT+1, 0) ;
                Cont_abs(SCREEN_RIGHT+1, SCREEN_BOTTOM+1) ;
                Cont_abs(0, SCREEN_BOTTOM+1) ;
                /* set the window */
                Set_window(1, SCREEN_BOTTOM,
                           1, SCREEN_RIGHT) ;
            }
        }
    }
}


_time_stamp(pad)
    PAD *pad;
{
    delete_item(pad,"time");
    append_item(pad,"time","1");
}




/*** end Serve_Xevent.c ***/

