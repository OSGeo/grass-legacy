#include "xgdisp.h"
#include "gis.h"


Pixel
#ifdef _NO_PROTO
queryraster(x, y, ret)
     int x, y;
     int ret;
#else
queryraster(int x, int y, int ret)
#endif
{
  struct Categories cats;
  int    catnum;
  char   catnumstr[20];
  char   *catname;
  double east, north;
  char   eaststr[30], northstr[30];
  int    objx, objy, objwidth, objheight;
  struct Cell_head *cellhd;
  CELL cmin, cmax;
  
  if (Global.selectedObjects->object->Obj.GeoFrame.gridOn &&
      Global.selectedObjects->object->Obj.GeoFrame.grid.labelon)
    {
      XgdSetupPlot(Global.selectedObjects->object,
		   (double)Global.selectedObjects->object->Obj.GeoFrame.grid.yoff,
		   (double)Global.selectedObjects->object->height,
		   (double)Global.selectedObjects->object->Obj.GeoFrame.grid.xoff,
		   (double)Global.selectedObjects->object->width);
      objx = Global.selectedObjects->object->x +
	Global.selectedObjects->object->Obj.GeoFrame.grid.xoff; 
      objy = Global.selectedObjects->object->y +
	Global.selectedObjects->object->Obj.GeoFrame.grid.yoff; 
      objwidth = Global.selectedObjects->object->width -
	Global.selectedObjects->object->Obj.GeoFrame.grid.xoff;
      objheight = Global.selectedObjects->object->height -
	Global.selectedObjects->object->Obj.GeoFrame.grid.yoff;
    } else {
      XgdSetupPlot(Global.selectedObjects->object,
		   (double)0.0,
		   (double)Global.selectedObjects->object->height,
		   (double)0.0,
		   (double)Global.selectedObjects->object->width);
      objx = Global.selectedObjects->object->x;
      objy = Global.selectedObjects->object->y;
      objwidth = Global.selectedObjects->object->width;
      objheight = Global.selectedObjects->object->height;
    }
  
  if (x < objx || x > objx + objwidth || y < objy || y > objy + objheight )
    return NULL;
  
  G_read_cats( Global.selectedObjects->object->Obj.GeoFrame.rname,
	      Global.selectedObjects->object->Obj.GeoFrame.rmapset,
	      &cats);
  
  catnum = getcatnum(0, x, y, objx, objy, objx + objwidth, objy + objheight,
		     Global.selectedObjects->object->Obj.GeoFrame.rname,
		     Global.selectedObjects->object->Obj.GeoFrame.rmapset);
  G_get_color_range (&cmin, &cmax, 
       &Global.selectedObjects->object->Obj.GeoFrame.colors);

  if (ret)
    return(Global.selectedObjects->object->Obj.GeoFrame.lookup_tbl[catnum - cmin]);
  
  sprintf (catnumstr, "%d", catnum);
  
  catname= (char *)G_get_cat(catnum, cats);
  
  XmTextSetString(qrastcatnumw,catnumstr);
  XmTextSetString(qrastcatnamew,catname);
  
  XgdPlotWhereEN(x,y,&east,&north);
  sprintf (eaststr, "%f", east);
  sprintf (northstr, "%f", north);
  XmTextSetString(qrastnorthw,northstr);
  XmTextSetString(qrasteastw,eaststr);
  
}
