/*

NOTES: 

drawXor is a GC that has as drawing function Xxor.
It is a global variable.

*/




/* --------------------------------------------------
   
   RuberBandTillPress: Rubber Band a Line Until a
                       Button Press.
   
   -------------------------------------------------- */
RuberBandTillPress(w,ix1,iy1,ix2,iy2, button)
     Window w;
     int *ix1, *iy1, *ix2, *iy2;
	 int *button;
{
  GC drawXorGC;
  XGCValues GC_values;
  unsigned long GC_value_mask;

  extern Display *dspl;

  XEvent event;
  XWindowAttributes oldAttributes;    /* just to put attribs. */
  long oldEventMask;                  /* old mask to restore  */

  GC_values.line_width = 2;
  GC_values.foreground = 1;
  GC_values.function = GXxor;
  GC_values_mask = GC_values.line_width | GC_values.foreground
						| GC_values.function;

  drawXorGC = XtGetGC(w, GCLineWidth | GCForeground | 
				GCFunction, &GC_values);

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


	switch(((XButtonPressedEvent *)(&event))->button)
	{
	case Button1: *button = 1;
					break;
	case Button2: *button = 2;
					break;
	case Button3: *button = 3;
					break;
	default : break;
	}

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
      if( verbose)
        printf("\nUnknown Event Detected in MoveNode\n");
      XSelectInput( dspl,w, ((unsigned long) oldEventMask));
      XPutBackEvent( dspl, &event);
      return(-1);
    }
  }
}

