#include "xgrass_dlib.h"
#include <X11/extensions/shape.h>
#include "gis.h"

void
#ifdef _NO_PROTO
_xgdReDrawSitePixmap(obj, s)
     XgdObject *obj;
     XgdSiteInfo *s;
#else
_xgdReDrawSitePixmap(XgdObject *obj, XgdSiteInfo *s)
#endif
{
  FILE *infile;
  char	msg[1024];
  int 	i;
  double  D_u_to_d_col();
  double  D_u_to_d_row();
  double  U_X, U_Y;
  int 	D_X, D_Y;
  XEvent event;

  event.type = Expose;
  event.xexpose.display = obj->display;
  event.xexpose.window = obj->window;

  if (D_do_conversions(&obj->Obj.GeoFrame.region, 0, obj->height,0,obj->width))
    G_fatal_error("Error in calculating conversions");
  
  infile = G_fopen_sites_old(s->sname, s->smapset);
  if (infile == NULL) {
    sprintf (msg, "can't open sites file [%s]", s->sname);
    G_fatal_error(msg);
  }
  
  XSendEvent(obj->display, obj->window, False, ExposureMask, &event); 
  
  i = 0;
  while(XgdNextSite(infile, &obj->Obj.GeoFrame.region, &U_X, &U_Y)) {
    D_X = (int)D_u_to_d_col(U_X) + obj->x - s->Site.pixdef.xhotspot;    
    D_Y = (int)D_u_to_d_row(U_Y) + obj->y - s->Site.pixdef.yhotspot;
    if (obj->Obj.GeoFrame.gridOn && obj->Obj.GeoFrame.grid.labelon){
      D_X += obj->Obj.GeoFrame.grid.xoff;
      D_Y += obj->Obj.GeoFrame.grid.yoff;
    }
    XMoveWindow(obj->display, s->Site.pixdef.pwin[i], D_X, D_Y);
    i++;
  }
  XSendEvent(obj->display, obj->window, False, ExposureMask, &event);

  fclose (infile);
}

void
#ifdef _NO_PROTO
XgdUpdateSiteStd(sinfo, icontype, size, width, color)
     XgdSiteInfo *sinfo;
     int icontype;
     int size;
     int width;
     Pixel color;
#else
XgdUpdateSiteStd(XgdSiteInfo *sinfo, int icontype, int size,
		 int width, Pixel color)
#endif
{
     XgdSetStdSiteIconType(sinfo, icontype);
     XgdSetStdSiteSize(sinfo, size);
     XgdSetStdSiteWidth(sinfo, width);
     XgdSetStdSiteColor(sinfo, color);
}

void 
#ifdef _NO_PROTO
XgdSetStdSiteIconType(sinfo, icontype)
XgdSiteInfo *sinfo;
int icontype;
#else
XgdSetStdSiteIconType(XgdSiteInfo *sinfo, int icontype)
#endif
{
    sinfo->Site.def.icontype = icontype;
}

void 
#ifdef _NO_PROTO
XgdSetStdSiteSize(sinfo, size)
XgdSiteInfo *sinfo;
int size;
#else
XgdSetStdSiteSize(XgdSiteInfo *sinfo, int size)
#endif
{
    sinfo->Site.def.size = size;
}

void
#ifdef _NO_PROTO
XgdSetStdSiteWidth(sinfo, width)
XgdSiteInfo *sinfo;
int width;
#else
XgdSetStdSiteWidth(XgdSiteInfo *sinfo, int width)
#endif
{
    sinfo->Site.def.width = width;
}

void
#ifdef _NO_PROTO
XgdSetStdSiteColor(sinfo, color)
XgdSiteInfo *sinfo;
Pixel color;
#else
XgdSetStdSiteColor(XgdSiteInfo *sinfo, Pixel color)
#endif
{
    sinfo->Site.def.color = color;
}

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

    _xgdDrawSiteStd(obj, obj->Obj.GeoFrame.sites.Tail);
    XCopyArea(obj->display, obj->Obj.GeoFrame.pixmap, obj->window,
	      obj->objgc, 0, 0, obj->width, obj->height, obj->x, obj->y);
    
    break;
  case XGD_SITE_PIXMAP:
    XgdUpdateSitePixmap(obj,sitefile, name, mapset);
    
    _xgdDrawSitePixmap(obj, window,
		       obj->Obj.GeoFrame.sites.Tail, name, mapset);
    break;
  }
}

void 
#ifdef _NO_PROTO
_xgdDrawSiteStd(obj, s)
     XgdObject *obj;
     XgdSiteInfo *s;
#else
_xgdDrawSiteStd(XgdObject *obj, XgdSiteInfo *s)
#endif
{
  char msg[1024];

  if (D_do_conversions(&obj->Obj.GeoFrame.region, 0, obj->height,0,obj->width))
    G_fatal_error("Error in calculating conversions");
  
  XSetForeground (obj->display,obj->objgc,s->Site.def.color);
  XSetLineAttributes(obj->display,obj->objgc,s->Site.def.width, LineSolid,
		     CapRound, JoinRound);
  
  switch (s->Site.def.icontype)
    {
    case CROSS:
      XgdSiteCross(obj, s);
      break;

    case DIAMOND:
      XgdSiteDiamond(obj, s);
      break;

    case RECT:
      XgdSiteBox(obj, s);
      break;
      
    case PLUS:
      XgdSitePlus(obj, s);
      break;
    }

  XSetLineAttributes(obj->display, obj->objgc,0, LineSolid,CapRound,JoinRound);
  XSetForeground(obj->display, obj->objgc, obj->fg);
}

void
#ifdef _NO_PROTO
XgdSiteCross(obj,s)
     XgdObject *obj;
     XgdSiteInfo *s;
#else
XgdSiteCross(XgdObject *obj, XgdSiteInfo *s)
#endif
{
  double U_X, U_Y;
  int    D_X, D_Y;
  double D_u_to_d_col() ;
  double D_u_to_d_row() ;
  FILE *infile = G_fopen_sites_old(s->sname, s->smapset);  

  while(XgdNextSite(infile, &obj->Obj.GeoFrame.region, &U_X, &U_Y)) {
    D_X = (int)D_u_to_d_col(U_X) ;
    D_Y = (int)D_u_to_d_row(U_Y) ;
    if (obj->Obj.GeoFrame.gridOn && obj->Obj.GeoFrame.grid.labelon){
      D_X += obj->Obj.GeoFrame.grid.xoff;
      D_Y += obj->Obj.GeoFrame.grid.yoff;
    }

    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X-s->Site.def.size),
	      (int)(D_Y-s->Site.def.size),
	      (int)(D_X+s->Site.def.size),
	      (int)(D_Y+s->Site.def.size) );
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
		obj->objgc,
	      (int)(D_X+s->Site.def.size), 
	      (int)(D_Y-s->Site.def.size),
	      (int)(D_X-s->Site.def.size), 
	      (int)(D_Y+s->Site.def.size) );
  }
  fclose(infile);
}

void
#ifdef _NO_PROTO
XgdSiteDiamond(obj,s)
     XgdObject *obj;
     XgdSiteInfo *s;
#else
XgdSiteDiamond(XgdObject *obj, XgdSiteInfo *s)
#endif
{
  double U_X, U_Y;
  int    D_X, D_Y;
  double D_u_to_d_col() ;
  double D_u_to_d_row() ;
  FILE *infile = G_fopen_sites_old(s->sname, s->smapset);  
  
  while(XgdNextSite(infile, &obj->Obj.GeoFrame.region, &U_X, &U_Y)) {
    D_X = (int)D_u_to_d_col(U_X);
    D_Y = (int)D_u_to_d_row(U_Y);
    if (obj->Obj.GeoFrame.gridOn && obj->Obj.GeoFrame.grid.labelon){
      D_X += obj->Obj.GeoFrame.grid.xoff;
      D_Y += obj->Obj.GeoFrame.grid.yoff;
    }
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X),
	      (int)(D_Y+s->Site.def.size), 
	      (int)(D_X+s->Site.def.size),
	      (int)D_Y   );
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X+s->Site.def.size), 
	      (int)D_Y, 
	      (int)D_X, 
	      (int)D_Y-s->Site.def.size);
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)D_X, 
	      (int)D_Y-s->Site.def.size, 
	      (int)D_X-s->Site.def.size, 
	      (int)D_Y );
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)D_X-s->Site.def.size, 
	      (int)D_Y, 
	      (int)D_X, 
	      (int)D_Y+s->Site.def.size );
  }
  fclose(infile);
}

void
#ifdef _NO_PROTO
XgdSiteBox(obj,s)
     XgdObject *obj;
     XgdSiteInfo *s;
#else
XgdSiteBox(XgdObject *obj, XgdSiteInfo *s)
#endif
{
  double U_X, U_Y;
  int    D_X, D_Y;
  double D_u_to_d_col() ;
  double D_u_to_d_row() ;
  FILE *infile = G_fopen_sites_old(s->sname, s->smapset);  
  
  while(XgdNextSite(infile, &obj->Obj.GeoFrame.region, &U_X, &U_Y)) {
    D_X = (int)D_u_to_d_col(U_X);
    D_Y = (int)D_u_to_d_row(U_Y);
    if (obj->Obj.GeoFrame.gridOn && obj->Obj.GeoFrame.grid.labelon){
      D_X += obj->Obj.GeoFrame.grid.xoff;
      D_Y += obj->Obj.GeoFrame.grid.yoff;
    }
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X-s->Site.def.size),
	      (int)(D_Y-s->Site.def.size), 
	      (int)(D_X-s->Site.def.size),
	      (int)(D_Y+s->Site.def.size) );
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X-s->Site.def.size), 
	      (int)(D_Y+s->Site.def.size), 
	      (int)(D_X+s->Site.def.size), 
	      (int)(D_Y+s->Site.def.size) );
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X+s->Site.def.size), 
	      (int)(D_Y+s->Site.def.size), 
	      (int)(D_X+s->Site.def.size),
	      (int)(D_Y-s->Site.def.size) );
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X+s->Site.def.size), 
	      (int)(D_Y-s->Site.def.size), 
	      (int)(D_X-s->Site.def.size), 
	      (int)(D_Y-s->Site.def.size) );
  }
  fclose(infile);
}


void
#ifdef _NO_PROTO
XgdSitePlus(obj,s)
     XgdObject *obj;
     XgdSiteInfo *s;
#else
XgdSitePlus(XgdObject *obj, XgdSiteInfo *s)
#endif
{
  double U_X, U_Y;
  int    D_X, D_Y;
  double D_u_to_d_col() ;
  double D_u_to_d_row() ;
  FILE *infile = G_fopen_sites_old(s->sname, s->smapset);  

  while(XgdNextSite(infile, &obj->Obj.GeoFrame.region, &U_X, &U_Y)) {
    D_X = (int)D_u_to_d_col(U_X) ;
    D_Y = (int)D_u_to_d_row(U_Y) ;
    if (obj->Obj.GeoFrame.gridOn && obj->Obj.GeoFrame.grid.labelon){
      D_X += obj->Obj.GeoFrame.grid.xoff;
      D_Y += obj->Obj.GeoFrame.grid.yoff;
    }
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X-s->Site.def.size),
	      (int)D_Y, 
	      (int)(D_X+s->Site.def.size),
	      (int)D_Y );
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)D_X, 
	      (int)D_Y-s->Site.def.size, 
	      (int)D_X, 
	      (int)D_Y+s->Site.def.size );
    
  }
  fclose(infile);
}

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

int
#ifdef _NO_PROTO
XgdNextSite(infile, region, U_X, U_Y)
     FILE   *infile;
     struct Cell_head *region;
     double *U_X;
     double *U_Y;
#else
XgdNextSite (FILE *infile, struct Cell_head *region,
             double *U_X, double *U_Y)
#endif
{
  char *desc;

  do {
    if (G_get_site (infile, U_X, U_Y, &desc) <= 0)
      return 0;
  } while(*U_X < region->west || *U_X > region->east ||
          *U_Y < region->south || *U_Y > region->north) ;

  return 1;
}

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
