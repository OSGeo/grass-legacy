/* Routine to process events for:
	
   RubberBanding routines: Calls Get_w_box
			   Left Button gives default sized box,
			   Middle Button allows user to choose
			   size with click and drag function
*/
#include <X11/Xlib.h>
#include <X11/Xutil.h>

extern Display *dpy;
extern Window  grwin;
  
Get_Region (x1,y1,x2,y2,button);

int 	*x1, *x2, *y1, *y2;
int 	*button;
{

 while (1)
      {
	XNextEvent(dpy, &event);
	/* if (CancelEvent (&event))*/
	    /* return 0; */
	if (event.type == ButtonPressedMask) && (event.window == grwin)
	   { 
	    /* Case Middle Button pressed: user selects size */
	    *x1 = event.xbutton.x;
	    *y1 = event.ybutton.y;
	    *button = event.xbutton.button;
	    Get_w_box(*x1,*y1,x2,y2,&button);;
	    return 1;
	   }
      }
}
