#include "xgrass_dlib.h"
#include "gis.h"

/*
 ***************************************************************************
 * XgdRasterImage - create a raster image from the specified object
 ***************************************************************************/
void
#ifdef _NO_PROTO
XgdRasterImage(obj, scrn)
     XgdObject      *obj;
     int             scrn;
#else
XgdRasterImage(XgdObject * obj,
               int scrn)
#endif
{
  char            buf[120];
  int             fd, row, col;
  int             crow = 0, ccol;
  int             lrow;
  register CELL  *cell;
  CELL            cmin, cmax;
  double          row_inc, col_inc;
  int             nrows, ncols;
  int             bitmap_pad;
  int		  objwidth, objheight;
  int 		  objx , objy; 
  
  
  nrows = obj->Obj.GeoFrame.region.rows;
  ncols = obj->Obj.GeoFrame.region.cols;
  
  if (obj->Obj.GeoFrame.gridOn && obj->Obj.GeoFrame.grid.labelon)
  {
    objwidth = obj->width - obj->Obj.GeoFrame.grid.xoff;
    objheight= obj->height - obj->Obj.GeoFrame.grid.yoff;
    objx = obj->Obj.GeoFrame.grid.xoff;
    objy = obj->Obj.GeoFrame.grid.yoff;
  } else {
    objwidth = obj->width ;
    objheight= obj->height ;
    objx = 0;
    objy = 0;
  }

  col_inc = (double) ncols / (double) objwidth;
  row_inc = (double) nrows / (double) objheight;

  bitmap_pad = _XgdGetBitmapPad(*__XGDVInfo);
  
  obj->Obj.GeoFrame.image = XCreateImage(obj->display, __XGDVInfo->visual,
			 __XGDVInfo->depth, ZPixmap, 0,
			 None, objwidth, objheight,
			 bitmap_pad, 0);
  
  if (!obj->Obj.GeoFrame.image)
    XgdError("Could not create image in XgdRasterImage");
  
  if ((obj->Obj.GeoFrame.image->data=XtMalloc(objheight * objwidth)) == NULL) {
    XgdWarning(" Insufficient memory to allocate raster image \n");
    return;
  }
  fd = G_open_cell_old(obj->Obj.GeoFrame.rname, obj->Obj.GeoFrame.rmapset);
  if (fd < 0) {
    sprintf(buf, "%s in %s -can't open cell file", obj->Obj.GeoFrame.rname, 
        obj->Obj.GeoFrame.rmapset);
    XgdError(buf);
  }
  cell = G_allocate_cell_buf();
  G_get_color_range (&cmin, &cmax, &obj->Obj.GeoFrame.colors);
  
  /* loop over rows */
  lrow = -1;
  for (row = 0; row < objheight; row++) {
    crow = (int) (((double) row * row_inc));
    if ( crow > nrows - 1 ) break;
    if (lrow != crow) {
      if (G_get_map_row(fd, cell, crow) < 0) {
	XgdError("Error reading Raster map.");
      }
    }
    lrow = crow;
    
    for (col = 1; col < objwidth - 1; col++) {
      ccol = (int) (((double) col * col_inc) + 0.5);
      XPutPixel(obj->Obj.GeoFrame.image, col, row,
		obj->Obj.GeoFrame.lookup_tbl[cell[ccol] - cmin]);
    }
    XPutImage(obj->display, obj->Obj.GeoFrame.pixmap, obj->objgc,
	      obj->Obj.GeoFrame.image, 0, row, objx, row+objy, objwidth, 1);
  }
  
  G_close_cell(fd);
  return;
}


