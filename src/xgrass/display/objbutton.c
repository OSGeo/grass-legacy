#include "xgdisp.h"

Widget
#ifdef _NO_PROTO
CreateObjectButtonPad(parent, string, columns, orientation)
Widget parent;
char *string;
int columns;
int orientation;
#else
CreateObjectButtonPad(
Widget parent,
char *string,
int columns,
int orientation)
#endif
{
    Widget rc;
    Widget w;
    XmString xms;
    Pixel fg, bg;
    int i;

    rc = XtVaCreateManagedWidget("button_pad_rc",
        xmRowColumnWidgetClass, parent, 
        XmNcolumns, columns,
	XmNorientation, orientation, 
        XmNpacking, XmPACK_TIGHT, 
        XmNisAligned, True,
        XmNentryAlignment, XmALIGNMENT_CENTER,
        XmNspacing, 0,
        NULL);

    XtVaGetValues(parent, XmNforeground, &fg, XmNbackground, &bg, NULL);

    for ( i = 0; i < XtNumber(objectButtonTable); i++ ) {
        Pixmap pixmap;
        XmString xms;

        if ( objectButtonTable[i].bitmap ) {
	    pixmap = XCreatePixmapFromBitmapData(Global.display,
		RootWindowOfScreen(Global.screenPtr), 
                objectButtonTable[i].bitmap,
		objectButtonTable[i].width, objectButtonTable[i].height,
		fg, bg, DefaultDepthOfScreen(Global.screenPtr));
	    w = XtVaCreateManagedWidget(objectButtonTable[i].label,
		xmPushButtonWidgetClass, rc, 
                XmNlabelType, XmPIXMAP,
                XmNlabelPixmap, pixmap,
		NULL);
        } else {
            xms = XmStringCreateSimple(objectButtonTable[i].label);
	    w = XtVaCreateManagedWidget(objectButtonTable[i].label,
		xmPushButtonWidgetClass, rc, 
                XmNlabelString, xms,
		NULL);
            XmStringFree(xms);
        }
        XtAddCallback(w, XmNactivateCallback, 
            SetDrawMode, objectButtonTable[i].type);
    }

    xms = XmStringCreateSimple("GeoFrame");
    w = XtVaCreateManagedWidget("geoframe_button",
        xmPushButtonWidgetClass, rc,
        XmNlabelString, xms,
        XmNalignment, XmALIGNMENT_CENTER,
        NULL);
    XmStringFree(xms);
    XtAddCallback(w, XmNactivateCallback, SetDrawMode, XGD_GEOFRAME);

    return rc;
}
