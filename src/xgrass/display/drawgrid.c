#include "xgdisp.h"
#include "gis.h"

void
#ifdef _NO_PROTO
DrawGrid(mode)
int mode;
#else
DrawGrid(int mode)
#endif
{
    XmString xms;

    if ( !(SelectedObjectCount() == 1 &&
         Global.selectedObjects->object->type == XGD_GEOFRAME )) {
         if ( SelectedObjectCount() > 1) {
	    XgWarningDialog(Global.applShell, "Select 1 GeoFrame Only");
	    } else {
            XgWarningDialog(Global.applShell, "No Selected GeoFrame");
	 }
	 return;
    }

    if ( XgdIsGridOn(Global.selectedObjects->object) ) {
	XgWarningDialog(Global.applShell, 
            "A grid already exists for\nthe selected GeoFrame");
        return;
    }

    xms = XmStringCreateSimple(GetStringFromMode(mode));
    XtVaSetValues(Global.messageArea, XmNlabelString, xms, NULL);
    XmStringFree(xms);
		
    if ( Global.selectedObjects->object ) {
    if (Global.gridbox != NULL && XtIsManaged(Global.gridbox)
	&& XmToggleButtonGetState(gridlabelonw) &&
	!Global.selectedObjects->object->Obj.GeoFrame.gridOn) {
      XmToggleButtonSetState(gridlabelonw, False, True);
      Global.gridattr.xoff = 0;
      Global.gridattr.yoff = 0;
      Global.gridattr.labelon = 0;
        
    }
    XgdUpdateGrid(Global.selectedObjects->object, True, 
	Global.gridattr.labelon,
	Global.gridattr.gridgap,
        Global.gridattr.spacing, Global.gridattr.color, 
        Global.gridattr.linewidth, Global.gridattr.linepattern, 
        Global.gridattr.fid, Global.gridattr.textcolor,
	Global.gridattr.xoff, Global.gridattr.yoff);
    }

}
