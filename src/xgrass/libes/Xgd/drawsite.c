#include "xgrass_dlib.h"

void 
#ifdef _NO_PROTO
XgdInitSite(obj, name, mapset, type)
     XgdObject *obj;
     char      *name;
     char      *mapset;
     int       type;
#else
XgdInitSite(XgdObject *obj,
	    char *name,
	    char *mapset,
	    int type)
#endif
{
  if (obj->type != XGD_GEOFRAME){
    XgdWarning("Object in XgdInitSite is not a GeoFrame");
    return;
  }
    
  if (obj->Obj.GeoFrame.sites.site == NULL) {
    obj->Obj.GeoFrame.sites.site =(XgdSiteInfo *)XtMalloc(sizeof(XgdSiteInfo) );
    obj->Obj.GeoFrame.sites.Tail = obj->Obj.GeoFrame.sites.site;
  } else {
    obj->Obj.GeoFrame.sites.Tail->next = (XgdSiteInfo *)XtMalloc(sizeof(XgdSiteInfo) );
    obj->Obj.GeoFrame.sites.Tail = obj->Obj.GeoFrame.sites.Tail->next;
  }
  obj->Obj.GeoFrame.sites.Tail->next = NULL;
  
  /* create room and stick the name and mapset in the structure */
  obj->Obj.GeoFrame.sites.Tail->sname = (char *)XtMalloc(sizeof (char)*strlen(name) + 1);
  obj->Obj.GeoFrame.sites.Tail->smapset= (char *)XtMalloc(sizeof (char)*strlen(mapset) + 1);
  
  strcpy(obj->Obj.GeoFrame.sites.Tail->sname, name);
  strcpy(obj->Obj.GeoFrame.sites.Tail->smapset, mapset);
  
  obj->Obj.GeoFrame.sites.Tail->type = type;
}

void 
#ifdef _NO_PROTO
XgdDrawSite(obj, window, name, mapset, sitefile)
     XgdObject *obj;
     Window    window;
     char      *name;
     char      *mapset;
     char      *sitefile;
#else
XgdDrawSite(XgdObject *obj, Window window, char *name, char *mapset, int type, char *sitefile)
#endif
{
  if (obj->type != XGD_GEOFRAME) {
    XgdWarning("Object in XgdDrawSite is not a GeoFrame.");
    return;
  }
  
  switch (obj->Obj.GeoFrame.sites.Tail->type) {
  case XGD_SITE_STANDARD:
    XgdUpdateSiteStd(obj->Obj.GeoFrame.sites.Tail, CROSS, 5, 1, 1L);

    _xgdDrawSiteStd(obj, obj->Obj.GeoFrame.sites.Tail);
    XCopyArea(obj->display, obj->Obj.GeoFrame.pixmap, obj->window,
	      obj->objgc, 0, 0, obj->width, obj->height, obj->x, obj->y);
    
    break;
  case XGD_SITE_PIXMAP:
    XgdUpdateSitePixmap(obj,sitefile, name, mapset);
    
    _xgdDrawSitePixmap(obj, window,
		       obj->Obj.GeoFrame.sites.Tail, name, mapset);
    break;
    case XGD_SITE_FREEHAND:
    XgdDrawSitefile(obj, name, mapset);
    break;
  }
}

