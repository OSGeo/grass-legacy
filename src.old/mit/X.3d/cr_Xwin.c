#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#define START_X 10
#define START_Y 20
#define WINDOW_WIDTH 700 
#define WINDOW_HEIGHT 550 
#define BORDER_WIDTH 1
#define KEY_STR_LENGTH 20

Display *the_display;
Window the_window;
int the_screen;
Window root_window;
XSizeHints size_hints;

GC the_gc;
XGCValues the_gc_values;
unsigned long the_gc_valuemask;
unsigned int mask;


create_Xwindow()
{
	if((the_display=XOpenDisplay("")) == NULL) 
	{
	G_fatal_error(" Can't open display\n");
	}

	the_screen=DefaultScreen(the_display);
	root_window=RootWindow(the_display,the_screen);

	size_hints.x=START_X;
	size_hints.y=START_Y;
	size_hints.width=WINDOW_WIDTH;
	size_hints.height=WINDOW_HEIGHT;
	size_hints.flags= PSize|PPosition;

	if((the_window=XCreateSimpleWindow
		(the_display, root_window,
		START_X,START_Y,WINDOW_WIDTH,
		WINDOW_HEIGHT,BORDER_WIDTH,
		BlackPixel(the_display,the_screen),
		WhitePixel(the_display,the_screen))) == 0)
	{
	G_fatal_error(" Window open failed\n");
	}

  	XSetStandardProperties(the_display,the_window,
			"My Window","My Icon",
	    		None,NULL,NULL,&size_hints);

	XMapWindow(the_display,the_window);

	XSelectInput(the_display,the_window,ExposureMask);

	the_gc=XCreateGC(the_display,the_window,None,
			   &the_gc_values);
}
