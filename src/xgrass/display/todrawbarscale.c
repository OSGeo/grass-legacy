#include "xgdisp.h"

void
#ifdef _NO_PROTO
todrawbarscale(obj, x, y)
    XgdObject      *obj;
    int         x, y;
#else
todrawbarscale(XgdObject *obj, int x, int y)
#endif
{
    if (Global.selectedObjects->object->Obj.GeoFrame.gridOn &&
        Global.selectedObjects->object->Obj.GeoFrame.grid.labelon)
        Global.barattr.winwidth =
        Global.selectedObjects->object->width - Global.selectedObjects->object->Obj.GeoFrame.grid.xoff;
    else
        Global.barattr.winwidth = Global.selectedObjects->object->width;


    obj->x = x;
    obj->y = y;
    obj->Obj.Barscale.gfobj = Global.selectedObjects->object;

    /* add it to the barscale array */
    if (Global.selectedObjects->object->Obj.GeoFrame.numbarscales != 0) {
        int i; 
        XgdObject *object = Global.selectedObjects->object;
        int oldcount = object->Obj.GeoFrame.numbarscales;
        XgdObject **newbs = 
            (XgdObject **)XtCalloc(oldcount + 1, sizeof(XgdObject *));
        
        for ( i = 0; i < object->Obj.GeoFrame.numbarscales; i++ ) 
            newbs[i] = object->Obj.GeoFrame.barscales[i];
	newbs[i] = obj;
        XtFree(object->Obj.GeoFrame.barscales);
        object->Obj.GeoFrame.barscales = newbs;
        object->Obj.GeoFrame.numbarscales++;
    } else {
        XgdObject *object = Global.selectedObjects->object;

        object->Obj.GeoFrame.barscales = (XgdObject **)
            XtCalloc(1, sizeof(XgdObject*));
        object->Obj.GeoFrame.barscales[0] = obj;
        object->Obj.GeoFrame.numbarscales++;
    }

    XgdUpdateBarscale(obj,
              Global.barattr.length,
              Global.barattr.intervals,
              Global.barattr.linewidth,
              Global.barattr.color,
              Global.barattr.style,
              Global.barattr.unit,
              Global.barattr.fid,
              Global.barattr.textcolor,
              Global.barattr.winwidth
        );

}
