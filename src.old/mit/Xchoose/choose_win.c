/*  %W%  %G%  */

#include <stdio.h>
/*
 *   Xchoose
 *
 *   Usage:  Xchoose name
 *           Xchoose     (choose with mouse if on tty)
 *
 *   Choose a window on the screen
 */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>


#define USAGE1 "name"
#define USAGE2 "   (to use mouse)"

Display *the_display;
int the_screen;

char cur_wind[32];

main(argc, argv)
	char *argv[];
{
	char orig_win[64] ;
	char cur_pad[64] ;
	int stat ;
	int button ;
	Window XD_get_cur_window();
	Window root_window, the_window, prev_window;
	Window Select_Window();
	Atom current_window;

	if (argc > 2)
	{
	fprintf(stderr, "Usage: %s %s\n",argv[0],USAGE1 );
	fprintf(stderr, "Usage: %s %s\n",argv[0],USAGE2 );
		exit(1);
	}

	/* Set the display to be the default display */
        if ((the_display = XOpenDisplay("")) == NULL)
        {
        printf(" can't open display\n");
        exit(-1);
        }

	the_screen = DefaultScreen(the_display);
	root_window = RootWindow(the_display, the_screen);


	prev_window = XD_get_cur_window(the_display, 
					the_screen);

	/* printf("\n prev_window = %u", prev_window); */


	if (argc == 2)
	{
	sscanf(argv[1], "%u", &the_window);
	}

	else
	/* select a window using the mouse */
	the_window = Select_Window(the_display, the_screen);

	/* printf("\n the_selected_window = %u",
		the_window); */

	sprintf(cur_wind, "%u", the_window);
	/* printf("\n current_window = %s", cur_wind); */

        current_window =  XInternAtom(the_display,
                                "current_window", True);

        XChangeProperty(the_display, root_window,
                current_window,
                XA_STRING, 8, PropModeReplace,
                cur_wind, 32);


	if(prev_window != the_window)
	{
        XSetWindowBorder(the_display, the_window,
                WhitePixel(the_display, the_screen));

	if(prev_window != NULL)
        XSetWindowBorder(the_display, prev_window,
                BlackPixel(the_display, the_screen));
	}


	XFlush(the_display);

}



Window Select_Window(dpy, screen)
     Display *dpy;
     int screen;
{
  int status;
  Cursor cursor;
  XEvent event;
  Window target_win;
  int buttons = 0;
  char *window_name;
  int found = 1;

  /* Make the target cursor */
  cursor = XCreateFontCursor(dpy, XC_crosshair);

  /* Grab the pointer using target cursor, 
	letting it room all over */

  status = XGrabPointer(dpy, RootWindow(dpy, screen), False,
           ButtonPressMask|ButtonReleaseMask, GrabModeSync,
           GrabModeAsync, None, cursor, CurrentTime);

  if (status != GrabSuccess) 
	G_fatal_error("Can't grab the mouse.");

  /* Let the user select a window... */
  while (found ||   (buttons != 0)) {
    /* allow one more event */
    XAllowEvents(dpy, SyncPointer, CurrentTime);
    XWindowEvent(dpy, RootWindow(dpy, screen),
             ButtonPressMask|ButtonReleaseMask, &event);

    switch (event.type) {
    case ButtonPress:
	
        target_win = event.xbutton.subwindow; /* window selec
ted */
	if(target_win == None)
	printf("\n Root");
	else{

	status = XFetchName(the_display, target_win, 
					&window_name);
	if(!strcmp(window_name, "Xnew")) found = 0;
	else printf("\n Click in Xnew window");
	    }

      buttons++;
      break;
    case ButtonRelease:
      if (buttons > 0) /* there may have been some down befor
e we started */
        buttons--;
       break;
    			} /* switch */
  } /* while */

  XUngrabPointer(dpy, CurrentTime);      /* Done with pointer
 */

  return(target_win);
}

