#include "xgrass_dlib.h"

/*
 ***************************************************************************
 * _XgdLinePatternExpose - redraw the line when the toolbox line buttons
 *      get exposed.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
_XgdLinePatternExpose(w, cld, cad)
Widget w;
_XgdLinePatternButtonData *cld;
XmDrawnButtonCallbackStruct *cad;
#else
_XgdLinePatternExpose(Widget w, _XgdLinePatternButtonData *cld, 
XmDrawnButtonCallbackStruct * cad)
#endif
{
    short hThick, sThick;
    Dimension ht, wid;

    XtVaGetValues(w, XmNhighlightThickness, &hThick, 
                     XmNshadowThickness, &sThick, 
                     XmNwidth, &wid, XmNheight, &ht, NULL);

    XDrawLine(XtDisplay(w), cad->window, cld->gc, sThick + hThick, 
        ht/2, wid /*- 2*(sThick + hThick)*/, ht/2);
}

/*
 ***************************************************************************
 * XgdCreateLinePatternPad - Create the line pattern widget, using
 *   the line pattern values store in __XGDlinePatternButtonTable.
 ***************************************************************************
 */
Widget
#ifdef _NO_PROTO
XgdCreateLinePatternPad(parent, string, cols, orientation, callback)
     Widget parent;
     char *string;
     int cols;
     int orientation;
     void (*callback)();
#else
XgdCreateLinePatternPad( Widget parent, char *string, int cols, 
    int orientation, void (*callback)())
#endif
{
  Widget frame;
  Widget rc;
  Widget w;
  Pixel fg, bg;
  int i, offset = 7;
  Widget caption;
  XmString xms;
  GC gc = NULL;
  XGCValues gcv;

  xms = XmStringCreateSimple(string);
  caption = XtVaCreateManagedWidget("line_caption", xbaeCaptionWidgetClass,
				    parent,
				    XmNlabelPosition, XbaePositionTop,
				    XmNlabelAlignment, XbaeAlignmentCenter,
				    XmNlabelString, xms,
				    NULL);
  XmStringFree(xms);

  XtVaSetValues(caption, XmNlabelOffset, -offset, NULL);
  
  frame = XtVaCreateManagedWidget("linepat_pad_frame", 
				  xmFrameWidgetClass, caption,
				  XmNmarginWidth, offset, 
				  XmNmarginHeight, offset, NULL);
  
  rc = XtVaCreateManagedWidget("linepat_pad_rc",
			       xmRowColumnWidgetClass, frame,
			       XmNcolumns, cols,
			       XmNorientation, orientation,
			       NULL);
  
  XtVaGetValues(parent, XmNforeground, &fg, XmNbackground, &bg, NULL);
  
  for ( i = 0; i < XtNumber(__XGDlinePatternButtonTable); i++ ) {
    XmString xms;
    
    if ( __XGDlinePatternButtonTable[i].dashes ) {
      _XgdLinePatternButtonData *lpd;
      unsigned long mask = GCFunction|GCForeground|GCBackground|GCLineWidth;
      XRectangle rect[1];
      short hThick, sThick;

      w = XtVaCreateManagedWidget(__XGDlinePatternButtonTable[i].label,
				  xmDrawnButtonWidgetClass, rc, 
				  NULL);

      XtVaGetValues(w, XmNhighlightThickness, &hThick, 
                       XmNshadowThickness, &sThick, NULL);
      XtVaSetValues(w, XmNwidth, 24 + 2*hThick + 2*sThick,
                       XmNheight, 24 + 2*hThick + 2*sThick, NULL);

      rect[0].x = sThick + hThick;
      rect[0].y = sThick + hThick;
      rect[0].width = 24;
      rect[0].height = 24;

      gcv.function = GXcopy;
      gcv.foreground = fg;
      gcv.background = bg;
      gcv.line_width = 3;
      gc = XCreateGC(XtDisplay(parent), DefaultRootWindow(XtDisplay(parent)), 
          mask, &gcv);
      XSetLineAttributes(XtDisplay(parent), gc, 0, LineOnOffDash,
	  CapButt, JoinMiter);
      XSetDashes(XtDisplay(parent), gc, 0, 
	  __XGDlinePatternButtonTable[i].dashes, 
	  __XGDlinePatternButtonTable[i].n);
      XSetClipRectangles(XtDisplay(parent), gc, 
	  0, 0, rect, 1, YXSorted);
      lpd = (_XgdLinePatternButtonData *)
          XtMalloc(sizeof(_XgdLinePatternButtonData));
      lpd->linePattern = &__XGDlinePatternButtonTable[i];
      lpd->gc = gc;
      XtAddCallback(w, XmNexposeCallback, _XgdLinePatternExpose, lpd);
    } else {
      xms = XmStringCreateSimple(__XGDlinePatternButtonTable[i].label);
      w = XtVaCreateManagedWidget(__XGDlinePatternButtonTable[i].label,
				  xmPushButtonWidgetClass, rc, 
				  XmNlabelString, xms,
				  NULL);
      XmStringFree(xms);
    }
    XtAddCallback(w, XmNactivateCallback, callback, i);
  }
  
  return caption;
}

