#include "xgdisp.h"

void
#ifdef _NO_PROTO
XgdUpdateSitePixmap(obj, sitefile, name, mapset)
XgdObject *obj;
char *sitefile;
char *name, *mapset;
#else
XgdUpdateSitePixmap(XgdObject *obj, char *sitefile, char *name, char *mapset)
#endif
{
  double U_X, U_Y;
  FILE *infile;
  int count;
  
  obj->Obj.GeoFrame.sites.Tail->Site.pixdef.pixmapfile= sitefile;
  
  count = 0;
  infile = G_fopen_sites_old(name, mapset); 
  while(XgdNextSite(infile, &obj->Obj.GeoFrame.region, &U_X, &U_Y)) 
    count++;

  obj->Obj.GeoFrame.sites.Tail->Site.pixdef.count= count;
  obj->Obj.GeoFrame.sites.Tail->Site.pixdef.xhotspot = 0;
  obj->Obj.GeoFrame.sites.Tail->Site.pixdef.yhotspot = 0;
  obj->Obj.GeoFrame.sites.Tail->Site.pixdef.pwin= (Window*) XtMalloc(sizeof(unsigned long)*count);
  
  fclose(infile);
}
