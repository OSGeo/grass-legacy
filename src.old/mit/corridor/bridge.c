#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/CompositeP.h>
#include <X11/CommandP.h>
#include <X11/LabelP.h>
#include <X11/Box.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Form.h>
#include <X11/Core.h>

extern Widget toplevel;
GC drawXorGC;
XGCValues the_GC_values;
unsigned long the_GC_value_mask;

void bridge(w, closure, callData)
	Widget w;
	caddr_t closure;
	caddr_t callData;
{

	int start_x, start_y, end_x, end_y;
	
	read_start_coors(XtWindow(w), &start_x, &start_y);
	RuberBand(XtWindow(w),&start_x, &start_y, 
				&end_x, &end_y);
}



read_start_coors(w,x,y)
	int *x, *y;
	Window w;
{
	
	XEvent event;
	XWindowAttributes oldAttributes;
	long oldEventMask;

	oldEventMask = oldAttributes.your_event_mask;
	XSelectInput(XtDisplay(toplevel), w, 
	((unsigned long) oldEventMask) | 	
				ButtonPressMask);

	the_GC_values.line_width = 2;
	the_GC_values.foreground = 0;
	the_GC_values.function = GXxor;
	the_GC_value_mask = the_GC_values.line_width|
				the_GC_values.function|
				the_GC_values.foreground;

	drawXorGC = XCreateGC(XtDisplay(toplevel), w, the_GC_value_mask,
			&the_GC_values);

	while(1)
	{
	XNextEvent(XtDisplay(toplevel), &event);
	

	if(event.type == ButtonPress)
	{
	*x = (int) ((XButtonPressedEvent *)(&event))->x;
	*y = (int) ((XButtonPressedEvent *)(&event))->y;
	break;
	}
	}
	
}

RuberBand( w, ix1, iy1, ix2, iy2)
     Window w;
     int *ix1, *iy1, *ix2, *iy2;
{
  extern GC drawXorGC;
  Display *dspl;
	
	

  XEvent event;
  XWindowAttributes oldAttributes;    /* just to put attribs. */
  long oldEventMask;                  /* old mask to restore  */

	dspl = XtDisplay(toplevel);
  /* Start Getting Moved Events */

  XGetWindowAttributes(dspl, w, &oldAttributes);
  oldEventMask = oldAttributes.your_event_mask;
  XSelectInput(dspl,w, ((unsigned long) oldEventMask) |
                     PointerMotionMask);
  

  /* Setup and Draw First Line */
  *ix2 = *ix1;
  *iy2 = *iy1;
  XDrawLine(dspl,w,drawXorGC, *ix1, *iy1, *ix2, *iy2);

  /* Start Rubber Banding the Line until Click */

  while( TRUE ) {

/*    XSync(dspl, 1);  Not Needed for Rubber Banding a Single Line */
    XNextEvent(dspl, &event);

    /* -- Button Press Events indicates the end of motion -- */
    if( event.type == ButtonPress ){
      XSelectInput(dspl,w,(unsigned long) oldEventMask);
      if( ((XButtonPressedEvent *)(&event))->window != w)
        XPutBackEvent(dspl, &event);
      /* erase the line so that further process can be done */
      XDrawLine(dspl,w,drawXorGC,*ix1,*iy1,*ix2,*iy2);
      if( ((XButtonPressedEvent *)(&event))->button == Button3)
        return(-1);
      else
        return(1);
    }
    /* -- Motion Events -- */
    else if( event.type == MotionNotify) {
      /* -- Motion Events in Structure Window -- */
      if ( ((XPointerMovedEvent *)(&event))->window == w) {
        XDrawLine(dspl,w,drawXorGC,*ix1,*iy1,*ix2,*iy2);
        *ix2 = ((XPointerMovedEvent *)(&event))->x;
        *iy2 = ((XPointerMovedEvent *)(&event))->y;
        XDrawLine(dspl,w,drawXorGC,*ix1,*iy1,*ix2,*iy2);
      }
      /* -- Motion Event on Another Window -- */
      else {
        XSelectInput(dspl,w,(unsigned long) oldEventMask);
        XPutBackEvent(dspl, &event);
        return(-1);
      }
    }
    else {
	/* if (verbose) */
        printf("\nUnknown Event Detected in MoveNode\n");
      XSelectInput( dspl,w, ((unsigned long) oldEventMask));
      XPutBackEvent( dspl, &event);
      return(-1);
    }
  }
}

