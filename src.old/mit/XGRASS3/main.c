#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h> 
#include <X11/Xatom.h>

/* standard C include file */
#include <stdio.h>

Display *the_display;
Window  root_window;
int the_screen;

Atom current_window;

static char cur_wind[32];
char temp[32];


main(argv, argc)
        int argc;
        char **argv;
{
	char* display_name = NULL;

	Atom actual_type;
	unsigned long nitems, remaining;
        unsigned char *one;
        int actual_format;


        /* connection to X server */
        if((the_display = XOpenDisplay(display_name)) == NULL)
	{
	fprintf(stderr,"Xnew : cannot connect to X server %s\n",
			XDisplayName(display_name));
	exit(-1);
	}

	/* getting screen size from display structure macro */
	the_screen = DefaultScreen(the_display);


	root_window = RootWindow(the_display, the_screen);


	/* set properties */
	strcpy(cur_wind, "");

	current_window = XInternAtom(the_display, 
			 "current_window", False);

	printf("\n id = %u", current_window);


	XChangeProperty(the_display, root_window, 
		current_window,
		XA_STRING, 8, PropModeReplace,
		cur_wind, 32);

	XGetWindowProperty(the_display,
               root_window, current_window, 0,
               320,
               False, XA_STRING, &actual_type,
               &actual_format, &nitems,
               &remaining, &one);

        sscanf(one, "%s", temp);

        printf("\n cur_window(str) = %s", temp);



}	/* end of MAIN program */
