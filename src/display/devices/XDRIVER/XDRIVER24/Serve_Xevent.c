#include <stdio.h>
#include "includes.h"
#include "../lib/graph.h"
#include "colors.h"
#include "pad.h"

extern int SCREEN_RIGHT, SCREEN_BOTTOM ;
extern XFontStruct *fontstruct;
extern Display *dpy;
extern Window grwin;
extern Pixmap bkupmap;
extern int backing_store;
extern GC gc;
extern unsigned SC_WID, SC_HITE;

extern PAD *curpad;
extern PAD *padlist;

int Service_Xevent (void)
{
    static int firstime = 1;
    Atom WM_DELETE_WINDOW;
    XEvent event;

    while( XPending(dpy)) { /* NOTE: This won't die if server terminates */
        XNextEvent(dpy, &event);
    /* On the first Expose events, write the grass display message. For
     * now, on subsequent expose copy the backup window to the display
     * window. */
    if ((event.type == Expose && event.xexpose.count == 0)
        || event.type == ConfigureNotify ) {
      if ( event.type == Expose ) {
        if (firstime) {
            /* Fill the window with the background color */
            XClearWindow(dpy, grwin);
            firstime = 0;
        } else 
        {
            XWindowAttributes xwa;

            /* Get the window's current attributes. */
	    
            if (XGetWindowAttributes(dpy, grwin, &xwa) == 0)
                return 1;

            SC_WID  = xwa.width;
            SC_HITE = xwa.height;
            SCREEN_RIGHT = xwa.width - 1;
            SCREEN_BOTTOM = xwa.height - 1;
            Set_window(1, xwa.height-1, 1, xwa.width-1);

            if (!backing_store)
                XCopyArea(dpy, bkupmap, grwin, gc, 0, 0, SC_WID, SC_HITE,
                    0, 0);
            firstime = 0;
        }
      } else if ( event.type == ConfigureNotify ) {
            /* if the window is not the same size do a d.frame -e */
            if ( event.xconfigure.width != SC_WID || 
                 event.xconfigure.height != SC_HITE ) {
                PAD *curpad;
                char buf[64];
                XWindowAttributes xwa;


                /* Get the window's current attributes. */
                if (XGetWindowAttributes(dpy, grwin, &xwa) == 0)
                    return 1;

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
		/* Handle backing store */
                if (!backing_store) {
/* fprintf(stderr,"Destroying old pixmap\n"); */
                    XFreePixmap(dpy, bkupmap);
                    bkupmap = XCreatePixmap(dpy, grwin, SC_WID, SC_HITE, 
                        xwa.depth);
                    XCopyArea(dpy, grwin, bkupmap, gc, 0, 0, (unsigned) SC_WID,
                            (unsigned) SC_HITE, 0, 0);
		}
            }
        }
    }
    if (event.type == ClientMessage)
    {
	WM_DELETE_WINDOW = XInternAtom(event.xclient.display, "WM_DELETE_WINDOW", False);
	if(event.xclient.data.l[0] == WM_DELETE_WINDOW)
	{
   	   Graph_Close();
	   exit(0);
	}
    }
    }

    return 0;
}

int _time_stamp (PAD *pad)
{
    delete_item(pad,"time");
    append_item(pad,"time","1");

    return 0;
}


/*** end Serve_Xevent.c ***/

