#include "xgdisp.h"

void 
#ifdef _NO_PROTO
RedrawArea(obj)
   XgdObject *obj;
#else
RedrawArea(XgdObject *obj)
#endif
{
ObjectList addlist=NULL, exclude=NULL;


    AddObjectToList(&exclude, obj);
    GetBoxAffectedObjects(XgdGetBBoxOfObject(obj), &addlist, &exclude);
    XgdUnDrawObject(obj, obj->bg, False);
    if (addlist)
       DrawObjectsInList(addlist);
} 

