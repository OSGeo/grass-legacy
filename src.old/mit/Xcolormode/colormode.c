/* @(#)colormode.c	2.1   6/26/87 */

/*
 *   Xcolormode
 *
 *   Usage:  Xcolormode mode=fixed/float
 *           Xcolormode offset=num
 *
 */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#define USAGE1	"fixed/float"

#include "gis.h"
#define MAIN
#include "options.h"

Display *the_display;
int the_screen;
Window the_window;
XClientMessageEvent event;

char color_mode[20];
Atom colormode;

main(argc, argv)
	int argc ;
	char **argv ;
{
	extern int stash_away() ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
	fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE1) ;
	exit(-1) ;
	}

	/* Set the display to be the default display */
        if ((the_display = XOpenDisplay("")) == NULL)
        {
        printf(" can't open display\n");
        return(-1);
        }

        the_screen = DefaultScreen(the_display);
        the_window = XD_get_cur_window(the_display,
                                the_screen);

	colormode = XInternAtom(the_display, 
				"colormode", True);

	event.type = ClientMessage;
        event.send_event = True;
        event.display = the_display;
        event.window = the_window;
        event.format = 8;
        event.message_type = XA_STRING;

	if(mode == FLOAT)
	{
	strcpy(color_mode, "float");
	strcpy((char *) (event.data.b), "float");
	}
	else
	{
	strcpy(color_mode, "fixed");
        strcpy((char *) (event.data.b), "fixed");
	}

	XSendEvent(the_display, the_window,
                True, PropertyChangeMask, &event);

	XChangeProperty(the_display, the_window, colormode,
                XA_STRING, 8, PropModeReplace,
                color_mode, 32);

	XFlush(the_display);

}
