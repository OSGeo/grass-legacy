#include "xgrass_dlib.h"
#include "gis.h"
#include "clrtbl.h"
#include "hcolr.h"

extern int      red_value[256], grn_value[256], blu_value[256];

/*
 ***************************************************************************
 * XgdDitherImage - dither the object
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdDitherImage(obj, scrn, name, mapset, chcat, lhcat)
     XgdObject *obj;
     int             scrn;
     char           *name, *mapset;
     int             chcat;
     int             lhcat;
#else
XgdDitherImage(XgdObject *obj,
	       int scrn,
	       char *name, char *mapset,
	       int chcat, int lhcat)
#endif
{
  char            buf[120];
  int             fd, row, col;
  register CELL  *cell;
  unsigned char  *red, *blu, *grn, *set;
  int             rowrdr;
  int             nrows, ncols;
  int             bitmap_pad;
  double          row_inc, col_inc;
  int             ccol, crow = 0;
  int             lrow;
  int             dim;


  nrows = obj->Obj.GeoFrame.region.rows;
  ncols = obj->Obj.GeoFrame.region.cols;

  col_inc = (double) ncols / (double) obj->width;
  row_inc = (double) nrows / (double) obj->height;

  bitmap_pad = _XgdGetBitmapPad(*__XGDVInfo);

  obj->Obj.GeoFrame.colormode = XGD_DITHERED_IMAGE;
  
  obj->Obj.GeoFrame.image = XCreateImage(obj->display, __XGDVInfo->visual,
                         __XGDVInfo->depth, ZPixmap, 0,
                         None, obj->width, obj->height,
                         bitmap_pad, 0);
  if (!obj->Obj.GeoFrame.image)
    XgdError("Could not create image in XgdRasterImage");

  if ((obj->Obj.GeoFrame.image->data = XtMalloc(obj->height * obj->width)) == NULL) {
    XgdWarning(" Insufficient memory to allocate raster image \n");
    return;
  }
  fd = G_open_cell_old(name, mapset);
  if (fd < 0) {
    sprintf(buf, "%s in %s -can't open cell file", name, mapset);
    XgdError(buf);
  }
  cell = G_allocate_cell_buf();
  
  for ( dim = 0; dim * dim * dim < obj->Obj.GeoFrame.fixedEntries; dim++);
  build_color_tables(dim);
  build_dither_tables();
  
  red = (unsigned char *) XtMalloc(ncols);
  grn = (unsigned char *) XtMalloc(ncols);
  blu = (unsigned char *) XtMalloc(ncols);
  set = (unsigned char *) XtMalloc(ncols);
  
  /* loop over rows */
  lrow = -1;
  for (row = 0; row < obj->height; row++) {
    crow = (int) (((double) row * row_inc) + 0.5);
    if ( crow > nrows - 1 ) break;
    if (lrow != crow) {
      if (G_get_map_row(fd, cell, crow) < 0) {
        XgdError("Error reading Raster map.");
      }

      G_lookup_colors(cell, red, grn, blu, set, ncols, &obj->Obj.GeoFrame.colors);
      rowrdr = row % 8;
      t_dither(red, grn, blu, rowrdr, 7, ncols);
    }

    lrow = crow;

    for (col = 0; col < obj->width; col++) {
      ccol = (int) (((double) col * col_inc) + 0.5);
      if (chcat >= 0 && chcat == cell[col] && chcat != lhcat) {
        XPutPixel(obj->Obj.GeoFrame.image, col, row, hglight);
      } else {
        int index = colornum(red[ccol], grn[ccol], blu[ccol]);
        XPutPixel(obj->Obj.GeoFrame.image, col, row, 
          obj->Obj.GeoFrame.lookup_tbl[index]);
      }
    }
    XPutImage(obj->display, obj->Obj.GeoFrame.pixmap, obj->objgc,
              obj->Obj.GeoFrame.image, 0, row, 0, row, obj->width, 1);
  }

  lhcat = chcat;
  G_close_cell(fd);
  return;
}
