#include "xgdisp.h"

void
#ifdef _NO_PROTO
SetUnits(w, cld, cad)
Widget w;
int cld;
XtPointer cad;
#else
SetUnits(Widget w, int cld, XtPointer cad)
#endif
{
    XmString xxms;
    XmString yxms;

    if ( Global.units == (int)cld ) return;

    switch((int)cld) {
        case XGD_UNITS_PIXELS:
            if ( Global.units == XGD_UNITS_MILLI ) {
                ClearRulerPixmaps();
		Global.units = (int)cld;
                DrawRulerPixmaps();
	        XCopyArea(Global.display, Global.hRuler, Global.hRulerWindow,
		     Global.rulerGC,
		     0, 0, Global.hWidth, 15, 0, 0);
	        XCopyArea(Global.display, Global.vRuler, Global.vRulerWindow,
		     Global.rulerGC,
		     0, 0, 15, Global.vHeight, 0, 0);
            }
            xxms = XmStringCreateSimple("X:");
            yxms = XmStringCreateSimple("Y:");
            XtVaSetValues(Global.xPosArea, XmNcolumns, 4, NULL);
            XtVaSetValues(Global.yPosArea, XmNcolumns, 4, NULL);
            break;
        case XGD_UNITS_INCHES:
            if ( Global.units == XGD_UNITS_MILLI ) {
                ClearRulerPixmaps();
		Global.units = (int)cld;
                DrawRulerPixmaps();
	        XCopyArea(Global.display, Global.hRuler, Global.hRulerWindow,
		     Global.rulerGC,
		     0, 0, Global.hWidth, 15, 0, 0);
	        XCopyArea(Global.display, Global.vRuler, Global.vRulerWindow,
		     Global.rulerGC,
		     0, 0, 15, Global.vHeight, 0, 0);
            }
            xxms = XmStringCreateSimple("X (in.):");
            yxms = XmStringCreateSimple("Y (in.):");
            XtVaSetValues(Global.xPosArea, XmNcolumns, 8, NULL);
            XtVaSetValues(Global.yPosArea, XmNcolumns, 8, NULL);
            break;
        case XGD_UNITS_MILLI:
            if ( Global.units != XGD_UNITS_MILLI ) {
                ClearRulerPixmaps();
		Global.units = (int)cld;
                DrawRulerPixmaps();
	        XCopyArea(Global.display, Global.hRuler, Global.hRulerWindow,
		     Global.rulerGC,
		     0, 0, Global.hWidth, 15, 0, 0);
	        XCopyArea(Global.display, Global.vRuler, Global.vRulerWindow,
		     Global.rulerGC,
		     0, 0, 15, Global.vHeight, 0, 0);
            }
            xxms = XmStringCreateSimple("X (mm.):");
            yxms = XmStringCreateSimple("Y (mm.):");
            XtVaSetValues(Global.xPosArea, XmNcolumns, 8, NULL);
            XtVaSetValues(Global.yPosArea, XmNcolumns, 8, NULL);
            break;
    }
    XtVaSetValues(Global.xPosAreaLabel, XmNlabelString, xxms, NULL);
    XtVaSetValues(Global.yPosAreaLabel, XmNlabelString, yxms, NULL);
    XmStringFree(xxms);
    XmStringFree(yxms);
    Global.units = (int)cld;
}

