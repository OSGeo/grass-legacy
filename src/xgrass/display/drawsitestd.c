#include "xgdisp.h"
#include "gis.h"

FILE *infile;

void 
#ifdef _NO_PROTO
_xgdDrawSiteStd(obj,sicontype, ssize, swidth, scolor, name, mapset)
     XgdObject *obj;
     int sicontype, ssize, swidth;
     Pixel scolor;
     char *name, *mapset;
#else
_xgdDrawsitestd(XgdObject *obj,
		int sicontype,
		int ssize,
		int swidth, 
		Pixel scolor,
		char *name,
		char *mapset)
#endif
{
  char msg[1024];

  infile= G_fopen_sites_old(name,mapset);
  if (infile==NULL) {
    sprintf (msg, "can't open sites file [%s]", name);
    G_fatal_error(msg);
  }

  if (D_do_conversions(&obj->Obj.GeoFrame.region, 0, obj->height,0,obj->width))
    G_fatal_error("Error in calculating conversions");

  XSetForeground (obj->display,obj->objgc,scolor);
  XSetLineAttributes(obj->display,obj->objgc,swidth, LineSolid,
		     CapRound, JoinRound);

  switch (sicontype)
    {
    case CROSS:
      XgdSiteCross(obj, ssize);
      break;

    case DIAMOND:
      XgdSiteDiamond(obj, ssize);
      break;

    case RECT:
      XgdSiteBox(obj, ssize);
      break;
      
    case PLUS:
      XgdSitePlus(obj, ssize);
      break;
    }

  XSetLineAttributes(obj->display,obj->objgc,0,
		     LineSolid,CapRound,JoinRound);

  fclose(infile);

}

XgdSiteCross(obj,size)
     XgdObject *obj;
     int 	size;
{
  double U_X, U_Y;
  int    D_X, D_Y;
  double D_u_to_d_col() ;
  double D_u_to_d_row() ;

  while(XgdNextSite(&obj->Obj.GeoFrame.region, &U_X, &U_Y)) {
    D_X = (int)D_u_to_d_col(U_X) ;
    D_Y = (int)D_u_to_d_row(U_Y) ;
    if (obj->Obj.GeoFrame.gridOn && obj->Obj.GeoFrame.grid.labelon){
      D_X += obj->Obj.GeoFrame.grid.xoff;
      D_Y += obj->Obj.GeoFrame.grid.yoff;
    }
    
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X-size),
	      (int)(D_Y-size),
	      (int)(D_X+size),
	      (int)(D_Y+size) );
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
		obj->objgc,
	      (int)(D_X+size), 
	      (int)(D_Y-size),
	      (int)(D_X-size), 
	      (int)(D_Y+size) );
  }
}

XgdSiteDiamond(obj, size)
     XgdObject *obj;
     int 	size;
{
  double U_X, U_Y;
  int    D_X, D_Y;
  double D_u_to_d_col() ;
  double D_u_to_d_row() ;
  
  while(XgdNextSite(&obj->Obj.GeoFrame.region, &U_X, &U_Y)){
    D_X = (int)D_u_to_d_col(U_X);
    D_Y = (int)D_u_to_d_row(U_Y);
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X),
	      (int)(D_Y+size), 
	      (int)(D_X+size),
	      (int)D_Y   );
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X+size), 
	      (int)D_Y, 
	      (int)D_X, 
	      (int)D_Y-size);
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)D_X, 
	      (int)D_Y-size, 
	      (int)D_X-size, 
	      (int)D_Y );
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)D_X-size, 
	      (int)D_Y, 
	      (int)D_X, 
	      (int)D_Y+size );
    
  }
}

XgdSiteBox(obj,size)
     XgdObject *obj;
     int 	size;
{
  double U_X, U_Y;
  int    D_X, D_Y;
  double D_u_to_d_col() ;
  double D_u_to_d_row() ;
  
  while(XgdNextSite(obj->Obj.GeoFrame.region, &U_X, &U_Y)){
    D_X = (int)D_u_to_d_col(U_X);
    D_Y = (int)D_u_to_d_row(U_Y);
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X-size),
	      (int)(D_Y-size), 
	      (int)(D_X-size),
	      (int)(D_Y+size) );
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X-size), 
	      (int)(D_Y+size), 
	      (int)(D_X+size), 
	      (int)(D_Y+size) );
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X+size), 
	      (int)(D_Y+size), 
	      (int)(D_X+size),
	      (int)(D_Y-size) );
    XDrawLine(obj->display,
	      obj->Obj.GeoFrame.pixmap,
	      obj->objgc,
	      (int)(D_X+size), 
	      (int)(D_Y-size), 
	      (int)(D_X-size), 
	      (int)(D_Y-size) );
  }
}


XgdSitePlus(obj,size)
XgdObject *obj;
int 	size;
{
double U_X, U_Y;
int    D_X, D_Y;
double D_u_to_d_col() ;
double D_u_to_d_row() ;

	while(XgdNextSite(&obj->Obj.GeoFrame.region, &U_X, &U_Y))
	{
	D_X = (int)D_u_to_d_col(U_X) ;
	D_Y = (int)D_u_to_d_row(U_Y) ;
	XDrawLine(obj->display,
		obj->Obj.GeoFrame.pixmap,
		obj->objgc,
		(int)(D_X-size),
		(int)D_Y, 
		(int)(D_X+size),
		(int)D_Y );
	XDrawLine(obj->display,
		obj->Obj.GeoFrame.pixmap,
		obj->objgc,
		(int)D_X, 
		(int)D_Y-size, 
		(int)D_X, 
		(int)D_Y+size );

	}
}


	

