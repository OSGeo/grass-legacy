#include "xgdisp.h" 

void
#ifdef _NO_PROTO
SetSite(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
SetSite(Widget w, XtPointer cld, XtPointer cad)
#endif
{
  Global.setSite = (int) cld;
  if (Global.setSite == XGD_SITE_FREEHAND) {
    if ( SelectedObjectCount()==1 &&
	Global.selectedObjects->object->type==XGD_GEOFRAME)
	return;	

       todrawoutpix();

}
}

