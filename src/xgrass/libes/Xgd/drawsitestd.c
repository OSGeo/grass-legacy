#include "xgrass_dlib.h"
#include "gis.h"

void 
#ifdef _NO_PROTO
_xgdDrawSiteStd(obj, s)
     XgdObject *obj;
     XgdSiteInfo *s;
#else
_xgdDrawsitestd(XgdObject *obj, XgdSiteInfo *s)
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

  XSetLineAttributes(obj->display, obj->objgc, 0, LineSolid,CapRound,JoinRound);

}

XgdSiteCross(obj,s)
     XgdObject *obj;
     XgdSiteInfo *s;
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

XgdSiteDiamond(obj, s)
     XgdObject *obj;
     XgdSiteInfo *s;
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

XgdSiteBox(obj, s)
     XgdObject *obj;
     XgdSiteInfo *s;
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


XgdSitePlus(obj,s)
     XgdObject *obj;
     XgdSiteInfo *s;
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


	

