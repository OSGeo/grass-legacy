#include "xgdisp.h"


void 
#ifdef _NO_PROTO
XgdDrawSitefile(obj, name, mapset)
     XgdObject *obj;
     char	*name, *mapset;
#else
_xgdDrawSitefile(XgdObject *obj, char *name, char *mapset)
#endif
{

  if (!sitefh.other)
    return;
  
  if (D_do_conversions(&obj->Obj.GeoFrame.region,
		       0,obj->height,0,obj->width))
    G_fatal_error("Error in calculating conversions");
  
  _xgdDrawsitefile(obj,name,mapset);
}

