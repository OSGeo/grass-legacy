#include "xgrass_dlib.h"

Widget
#ifdef _NO_PROTO
XgdCreateFillPatternPad(parent, string, cols, orientation, callback)
Widget parent;
char *string;
int cols;
int orientation;
void (*callback)();
#else
XgdCreateFillPatternPad( Widget parent, char *string, int cols, 
    int orientation, void (*callback)())
#endif
{
    Widget frame;
    Widget rc;
    Widget w;
    Pixel fg, bg;
    int i;
    Widget caption;
    XmString xms;

    xms = XmStringCreateSimple(string);
    caption = XtVaCreateManagedWidget("fill_caption", xbaeCaptionWidgetClass,
				    parent,
				    XmNlabelPosition, XbaePositionTop,
				    XmNlabelAlignment, XbaeAlignmentCenter,
				    XmNlabelOffset, -7,
				    XmNlabelString, xms,
				    NULL);
    XmStringFree(xms);
  
    frame = XtVaCreateManagedWidget("fillpat_pad_frame", 
				  xmFrameWidgetClass, caption, 
                                  XmNmarginWidth, 7,
                                  XmNmarginHeight, 7,
                                  NULL);
  
    rc = XtVaCreateManagedWidget("button_pad_rc",
        xmRowColumnWidgetClass, frame, 
        XmNcolumns, cols,
	XmNorientation, orientation, NULL);

    XtVaGetValues(parent, XmNforeground, &fg, XmNbackground, &bg, NULL);

  for ( i = 0; i < XtNumber(__XGDfillPatternButtonTable); i++ ) {
        Pixmap pixmap;
        XmString xms;

        if ( __XGDfillPatternButtonTable[i].bitmap ) {
	    pixmap = XCreatePixmapFromBitmapData(XtDisplay(parent),
		RootWindowOfScreen(XtScreen(parent)), 
                __XGDfillPatternButtonTable[i].bitmap,
		__XGDfillPatternButtonTable[i].width, 
                __XGDfillPatternButtonTable[i].height,
		fg, bg, DefaultDepthOfScreen(XtScreen(parent)));
	    w = XtVaCreateManagedWidget(__XGDfillPatternButtonTable[i].label,
		xmPushButtonWidgetClass, rc, 
                XmNlabelType, XmPIXMAP,
                XmNlabelPixmap, pixmap,
		NULL);
        } else {
            xms = XmStringCreateSimple(__XGDfillPatternButtonTable[i].label);
	    w = XtVaCreateManagedWidget(__XGDfillPatternButtonTable[i].label,
		xmPushButtonWidgetClass, rc, 
                XmNlabelString, xms,
		NULL);
            XmStringFree(xms);
        }
        XtAddCallback(w, XmNactivateCallback, callback, i);
    }

    return caption;
}
  
