#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <X11/cursorfont.h>

#define START_X 10
#define START_Y 20
#define WINDOW_WIDTH 600 
#define WINDOW_HEIGHT 750 
#define BORDER_WIDTH 1
#define KEY_STR_LENGTH 20

Display *the_display;
Window the_window;
int the_screen;
Window root_window;
XSizeHints size_hints;
XEvent the_event;

GC the_gc;
XGCValues the_gc_values;
unsigned long the_gc_valuemask;
unsigned int mask;

Cursor the_cursor;

main(argc,argv)
int argc;
char *argv[];
{
	unsigned int cur_font;
  	int i;
	int  end_x, end_y;

	sscanf(argv[1],"%u",(char *) &cur_font);

  if((the_display=XOpenDisplay("")) == NULL) {
    printf("in %s : can't open display\n",argv[0]);
    return(-1);
  }
  the_screen=DefaultScreen(the_display);
  root_window=RootWindow(the_display,the_screen);


  size_hints.x=START_X;
  size_hints.y=START_Y;
  size_hints.width=WINDOW_WIDTH;
  size_hints.height=WINDOW_HEIGHT;
  size_hints.flags=USSize|USPosition;

  if((the_window=XCreateSimpleWindow(the_display,
		  root_window,
		  START_X,START_Y,WINDOW_WIDTH,WINDOW_HEIGHT,BORDER_WIDTH,
	          BlackPixel(the_display,the_screen),
                  WhitePixel(the_display,the_screen))) == 0) {
    printf("in %s: window open failed\n",argv[0]);
    return(-1);
  }

  XSetStandardProperties(the_display,the_window,"My Window","My Icon",
			 None,argv,argc,&size_hints);
  XMapWindow(the_display,the_window);

  XSelectInput(the_display,the_window,ExposureMask|ButtonPressMask);

  the_cursor=XCreateFontCursor(the_display,cur_font);

  XDefineCursor(the_display,the_window,the_cursor);

/*
  the_gc_values.line_width = 5;
  the_gc_values.line_style = LineDoubleDash;
  the_gc_valuemask = GCLineWidth|GCLineStyle; 
*/

  the_gc=XCreateGC(the_display,the_window,None,
		   &the_gc_values);

  while(1) {
    XNextEvent(the_display,&the_event);
    if(the_event.type == ButtonPress)
      if(((XButtonEvent *)&the_event)->button == 2)
	{
	/* rubber_band(((XButtonEvent *)&the_event)->x,
		    ((XButtonEvent *)&the_event)->y); */
	RubberBandTillPress(the_window,
	((XButtonEvent *) &the_event)->x,
	((XButtonEvent *) &the_event)->y,
	&end_x, &end_y);
	}	/* end of rubber processing */
      else
	;
    else;
      /* XDrawLine(the_display,the_window,the_gc,8,8,400,400); */
  }
}



rubber_band(start_x, start_y)
	int start_x, start_y;
{

	int rootx,rooty,new_x,new_y,pre_x,pre_y;
	Window root, child;

	pre_x = start_x - 10;
	pre_y = start_y - 10;

	again:
	XQueryPointer(the_display,the_window,&root,&child,
			&rootx,&rooty,&new_x,&new_y,&mask);

	if(mask && new_x>0 && new_y>0 && 
		new_x<size_hints.width && new_y<size_hints.height)

	{

	XSetForeground(the_display,the_gc,WhitePixel(the_display,the_screen));
	XDrawLine(the_display, the_window, the_gc, start_x, start_y,pre_x,pre_y);
	XFlush(the_display);
	XSetForeground(the_display,the_gc,BlackPixel(the_display,the_screen));


	XDrawLine(the_display, the_window, the_gc, start_x, start_y, new_x, new_y);
	pre_x = new_x;
	pre_y = new_y;

	goto again;
	}

	
}

