#include "xgdisp.h"

void
#ifdef _NO_PROTO
HorizontalSliderMoved(w, cld, cad)
Widget w;
int cld;
XmScrollBarCallbackStruct * cad;
#else
HorizontalSliderMoved(Widget w, int cld, XmScrollBarCallbackStruct * cad)
#endif
{
    if ( Global.lastX == cad->value ) return;

    /* Move the drawing area to the appropriate place */
    XtMoveWidget(Global.drawArea,  -cad->value, -Global.lastY);
    /* configure the ruler to match the draw area */
    XtConfigureWidget(Global.hRulerWidget, 15, 0, 
	Global.hWidth - cad->value, 15, 0);
    /* Save the last position */
    Global.lastX = cad->value;
    /* Redraw the hRuler */
    XCopyArea(Global.display, Global.hRuler, Global.hRulerWindow,
        Global.rulerGC, cad->value, 0, 
        Global.hWidth - (cad->value), 15, 0, 0);

}

void
#ifdef _NO_PROTO
VerticalSliderMoved(w, cld, cad)
Widget w;
int cld;
XmScrollBarCallbackStruct * cad;
#else
VerticalSliderMoved(Widget w, int cld, XmScrollBarCallbackStruct * cad)
#endif
{
    if ( Global.lastY == cad->value ) return;

    /* Move the drawing area to the appropriate place */
    XtMoveWidget(Global.drawArea, -Global.lastX, -cad->value);
    /* configure the ruler to match the draw area */
    XtConfigureWidget(Global.vRulerWidget, 0, 15, 15, 
	Global.vHeight - cad->value, 0);
    /* Save the last position */
    Global.lastY = cad->value;

    /* Redraw the vRuler */
    XCopyArea(Global.display, Global.vRuler, Global.vRulerWindow,
        Global.rulerGC, 0, cad->value, 
        15, Global.vHeight - cad->value, 0, 0);
}

void 
#ifdef _NO_PROTO
SetScrollBars(w,h)
int w, h;
#else
SetScrollBars(int w, int h)
#endif
{
    int hSize, vSize;

    XmScrollBarCallbackStruct sbcs;

    sbcs.value = 0;
    sbcs.reason = XmCR_VALUE_CHANGED;

    if ( w > Global.hWidth ) {
        hSize = Global.hWidth;
	XtVaSetValues(Global.hsb, XmNvalue, 0, NULL);
	HorizontalSliderMoved(Global.hsb, NULL, &sbcs);
    } else {
        hSize = w;
        if ( Global.lastX + w > Global.hWidth ) {
	    XtVaSetValues(Global.hsb, XmNvalue, 0, NULL);
	    HorizontalSliderMoved(Global.hsb, NULL, &sbcs);
	}
    }
    XtVaSetValues(Global.hsb, XmNsliderSize, hSize, NULL);
    if ( h > Global.vHeight ) {
        vSize = Global.vHeight;
	XtVaSetValues(Global.vsb, XmNvalue, 0, NULL);
       VerticalSliderMoved(Global.vsb, NULL, &sbcs);
    } else {
        vSize = h;
        if ( Global.lastY + h > Global.vHeight ) {
	    XtVaSetValues(Global.vsb, XmNvalue, 0, NULL);
	   VerticalSliderMoved(Global.vsb, NULL, &sbcs);
        }
    }
    XtVaSetValues(Global.vsb, XmNsliderSize, vSize, NULL);
}

