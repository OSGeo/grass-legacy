#include "xgrass_dlib.h"
#include <X11/extensions/shape.h>

void
#ifdef _NO_PROTO
_xgdDrawSitePixmap(obj, window, s, name, mapset, flag)
     XgdObject *obj;
     Window    window;
     XgdSiteInfo *s;
     char	*name, *mapset;
     Boolean flag;
#else
_xgdDrawSitePixmap(XgdObject *obj, Window window,
		   XgdSiteInfo *s, char *name, char *mapset, Boolean flag)
#endif
{
  int     xhotspot, yhotspot;
  XpmIcon	view;
  
  if (D_do_conversions(&obj->Obj.GeoFrame.region,
		       0,obj->height,0,obj->width))
    G_fatal_error("Error in calculating conversions");
  
  view.attributes.valuemask	= XpmColormap;
  view.attributes.colormap	= __XGDColormap;
  XpmReadFileToPixmap(obj->display,obj->window,
				    s->Site.pixdef.pixmapfile,
				    &view.pixmap,&view.mask,
				    &view.attributes);
  if (view.pixmap == NULL){
    XgdWarning("Invalid pixmap file");
    return;
  }
  
  if (view.attributes.valuemask & XpmHotspot){
      xhotspot = view.attributes.x_hotspot ;
      yhotspot = view.attributes.y_hotspot ;
    } else {
      xhotspot = 0;
      yhotspot = 0;
    }
  
  XgdSitePixmap(obj, window, view, xhotspot, yhotspot, name, mapset);

  XFreePixmap (obj->display, view.pixmap);
  if (view.mask)
    XFreePixmap (obj->display, view.mask);
  XpmFreeAttributes(&view.attributes);
  
  
  obj->Obj.GeoFrame.sites.Tail->Site.pixdef.xhotspot = xhotspot; 
  obj->Obj.GeoFrame.sites.Tail->Site.pixdef.yhotspot = yhotspot; 
}

void 
#ifdef _NO_PROTO
XgdSitePixmap(obj, win, view, xhotspot, yhotspot, name, mapset)
     XgdObject *obj;
     Window win;
     XpmIcon view;
     int xhotspot, yhotspot;
     char *name, *mapset;
#else
XgdSitePixmap(XgdObject *obj,
	      Window win,
	      XpmIcon view,
	      int xhotspot,
	      int yhotspot,
	      char *name,
	      char *mapset)
#endif
{
  double U_X, U_Y;
  int D_X, D_Y;
  double D_u_to_d_col();
  double D_u_to_d_row();
  int     count;
  char	msg[1024];
  FILE	*infile;

  infile = G_fopen_sites_old(name,mapset);
  if (infile==NULL){
    sprintf (msg, "can't open sites file [%s]",name);
    G_fatal_error(msg);
  }
  
  count = 0;
  while (XgdNextSite(infile, &obj->Obj.GeoFrame.region, &U_X, &U_Y) ) {
    D_X = (int)D_u_to_d_col(U_X) + obj->x ;
    D_Y = (int)D_u_to_d_row(U_Y) + obj->y ;
    if (obj->Obj.GeoFrame.gridOn && obj->Obj.GeoFrame.grid.labelon){
      D_X += obj->Obj.GeoFrame.grid.xoff;
      D_Y += obj->Obj.GeoFrame.grid.yoff;
    }
    obj->Obj.GeoFrame.sites.Tail->Site.pixdef.pwin[count] =
      XCreateSimpleWindow(obj->display,win, D_X-xhotspot,
			  D_Y-yhotspot,
			  view.attributes.width,
			  view.attributes.height, 0, 0, 0);
    XSetWindowBackgroundPixmap(obj->display,
			       obj->Obj.GeoFrame.sites.Tail->Site.pixdef.pwin[count], 
			       view.pixmap);	
    
    if (view.mask)
      XShapeCombineMask(obj->display, 
			obj->Obj.GeoFrame.sites.Tail->Site.pixdef.pwin[count],
			ShapeBounding, 0, 0, 
			view.mask, ShapeSet);
    
    XMapWindow(obj->display, 
	       obj->Obj.GeoFrame.sites.Tail->Site.pixdef.pwin[count]);
    count++;
  }
}
