#include "xgrass_dlib.h"

void
#ifdef _NO_PROTO
XgdDrawLegend(obj, pix)
     XgdObject *obj;
     Pixmap    pix;
#else
XgdDrawLegend(XgdObject *obj, Pixmap pix)
#endif
{
  int gap;
  int catsPerCol = 0;
  int numCats = 0;
  int i, j, y = 0;
  int height, width;
  char *name;
  int curcat = 0;
  int x = 0;
  int colWidth = 0;
  int cols = obj->Obj.Legend.numCols;
  
  if (!obj->Obj.Legend.font)
    obj->Obj.Legend.font = XQueryFont(obj->display,
                 XGContextFromGC(obj->objgc));
  
  if (obj->Obj.Legend.displaySelected){
    if (!obj->Obj.Legend.catsToList)
      numCats = 0;
    else
      for (numCats = 0; obj->Obj.Legend.catsToList[numCats]; numCats++);
  } else {
    numCats = obj->Obj.Legend.cats.num + 1;
  }

  height = obj->Obj.Legend.font->ascent + obj->Obj.Legend.font->descent;

  for (j = 0; j < obj->Obj.Legend.numCols; j++){
    catsPerCol = (numCats/cols);
    if (obj->height){
      if (catsPerCol == 1)
	gap = 0;
      else
	gap = (obj->height - catsPerCol * height)/(catsPerCol - 1);
      if (gap < 0)
	gap = 0;
    } else {
      gap = 0;
    }
    i = 0;
    while (i < catsPerCol){
      name = G_get_cat(curcat, &obj->Obj.Legend.cats);
      
      if (IsCatInList(obj, name)){
	XSetForeground(obj->display, obj->objgc,
		    obj->Obj.Legend.geoFrame->Obj.GeoFrame.lookup_tbl[curcat]);
	
	XFillRectangle(obj->display, pix ? pix : obj->window, obj->objgc,
		       obj->x + x,
		       obj->y + y, 9, height - 1);
	
	XSetForeground(obj->display, obj->objgc, obj->Obj.Legend.brdrcolor);
	XDrawRectangle(obj->display, pix?pix:obj->window, obj->objgc,
		       obj->x + x,
		       obj->y + y, 9, height - 1);
	
	XSetForeground(obj->display, obj->objgc, obj->fg);
	if (obj->Obj.Legend.toDisplay == XGD_DISPLAY_CAT_NUMS){
	  char str[5];
	  
	  sprintf(str, "%d", curcat);
	  name = str;
	} else if (obj->Obj.Legend.toDisplay == XGD_DISPLAY_CAT_NONE){
	  name = "";
	}
	width = XTextWidth(obj->Obj.Legend.font, name, strlen(name));
	XDrawString(obj->display, pix?pix:obj->window, obj->objgc,
		    obj->x + x + 20,
		    obj->y + y + height - 2 - obj->Obj.Legend.font->descent,
		    name, strlen(name));
	y += (height + gap);
	if (width + 20 > colWidth)
	  colWidth = width + 20;
	++i;
      }
      ++curcat;
    }
    --cols;
    numCats -= catsPerCol;
    x += colWidth + 5;
    if (j != obj->Obj.Legend.numCols - 1){
      y = 0;
    }
  }
  obj->width = x;
  
  if (!gap)
    obj->height = y;
  
  if (obj->bbox)
    XgdUpdateBoxForObject(obj, obj->y, obj->y + obj->height,
              obj->x, obj->x + obj->width);
  else
    XgdCreateBoxForObject(obj);

  XgdConfigureResizeHandles(obj);
}
