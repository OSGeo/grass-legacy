#include "xgdisp.h"

char *
#ifdef _NO_PROTO
GetStringFromMode(mode)
unsigned int mode;
#else
GetStringFromMode( unsigned int mode)
#endif
{
    if ( mode == XGD_MODE_DRAW ) {
        switch(Global.drawType) {
        case XGD_SQUARE:
            return "Draw Square";
        case XGD_RECTANGLE:
            return "Draw Rectangle";
        case XGD_CIRCLE:
            return "Draw Cirle";
        case XGD_ELLIPSE:
            return "Draw Ellipse";
        case XGD_POLYLINE:
            return "Draw Polyline";
        case XGD_POLYGON:
            return "Draw Polygon";
        case XGD_OPEN_INTERP_SPLINE:
            return "Draw Open Spline";
        case XGD_CLOSED_INTERP_SPLINE:
            return "Draw Close Spline";
        case XGD_OPEN_APPROX_SPLINE:
            return "Draw Open Approximated Spline";
        case XGD_CLOSED_APPROX_SPLINE:
            return "Draw Closed Approximated Spline";
        case XGD_GEOFRAME:
            return "Draw Geoframe";
        case XGD_LABEL:
            return "Draw Label";
        case XGD_LEGEND:
            return "Draw Legend";
        case XGD_GRID:
            return "Draw Grid";
        case XGD_BARSCALE:
            return "Draw Barscale";
        case XGD_HISTOGRAM:
            return "Draw Histogram";
        }
    }
    return ModeStringTable[mode];
}

void
#ifdef _NO_PROTO
SetMode(mode)
unsigned int mode;
#else
SetMode(unsigned int mode)
#endif
{
    XmString xms;

    Global.mode = mode;
    xms = XmStringCreateSimple(GetStringFromMode(Global.mode));
    XtVaSetValues(Global.messageArea, XmNlabelString, xms, NULL);
    XmStringFree(xms);
}

void
#ifdef _NO_PROTO
SetDrawMode(w, cld, cad)
Widget w;
int cld;
XtPointer cad;
#else
SetDrawMode( Widget w, int cld, XtPointer cad)
#endif
{
  Global.drawType = cld;

  /* Just draw it if its one of these guys */
  
  switch ((int)cld)
    {
    case XGD_GRID:
      DrawGrid(XGD_MODE_DRAW);
      SetMode(XGD_MODE_SELECT);
      return;
    case XGD_LEGEND:
      if (SelectedObjectCount() != 1){
	XgWarningDialog(Global.applShell, "Select 1 GeoFrame");
	return;
      } else if (Global.selectedObjects->object->type != XGD_GEOFRAME){
	XgWarningDialog(Global.applShell,
			"You must have a GeoFrame slected to use a legend");
	return;
      } else if (!Global.selectedObjects->object->Obj.GeoFrame.rname) {
	XgWarningDialog(Global.applShell, "No map is currently displayed");
	return;
      } else if (Global.selectedObjects->object->Obj.GeoFrame.legend){
	XgWarningDialog(Global.applShell,
			"A legend already exists for this Geoframe.");
	return;
      }
      
      break;
    case XGD_BARSCALE:
      if (SelectedObjectCount() != 1){
	XgWarningDialog(Global.applShell, "Select 1 GeoFrame");
	return;
      } else if (Global.selectedObjects->object->type != XGD_GEOFRAME){
	XgWarningDialog(Global.applShell,
			"You must have a GeoFrame slected to use a bar scale");
	return;
      }      
      break;
    }
  SetMode(XGD_MODE_DRAW);
}
