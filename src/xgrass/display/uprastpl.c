#include "xgdisp.h"

void 
#ifdef _NO_PROTO
UpdateRasterPanel()
#else
UpdateRasterPanel(void)
#endif
{
    XmTextFieldSetString(qrastnamew, Global.selectedObjects->object->Obj.GeoFrame.rname);
    XmTextFieldSetString(qrastmapsetw, Global.selectedObjects->object->Obj.GeoFrame.rmapset);
    XmTextFieldSetString(qrastnorthw, " ");
    XmTextFieldSetString(qrasteastw, " ");
    XmTextFieldSetString(qrastcatnamew, " ");
    XmTextFieldSetString(qrastcatnumw, " ");
}

