#include "xgrass_dlib.h"

#define MONO(rd,gn,bl) (((rd)*11 + (gn)*16 + (bl)*5) >> 5)

Boolean
#ifdef _NO_PROTO
_XgdGetFirstAvailableLutIndex(dpy, cmap, index)
    Display *dpy;
    Colormap cmap;
    int *index;
#else
_XgdGetFirstAvailableLutIndex(Display *dpy, Colormap cmap, int *index)
#endif
{
    int i;


    for ( i = 0; i < __XGDTotalCells; i++) {
        if ( __XGDLut[i].mode == XG_LUT_MODE_RW &&
             __XGDLut[i].status == XG_LUT_STATUS_AVAILABLE ) {
            /* allocate the color for use */
            if ( XAllocColorCells(dpy, cmap, 0, NULL, 0, 
                           &(__XGDLut[i].pixel), 1) == 0 ) {
                XColor q;

		q.pixel = (Pixel)__XGDLut[i].pixel;
		XQueryColor(dpy, cmap, &q);

                /* oops, something changed since we bulit the lut */
                /* make this a readonly color and obtain its info */
                __XGDLut[i].mode = XG_LUT_MODE_RO;
                __XGDLut[i].red = q.red;
                __XGDLut[i].green = q.green;
                __XGDLut[i].blue = q.blue;
            } else {
		*index = i;
		return True;
            }
        }
    }
    return False;
}

void
#ifdef _NO_PROTO
XgdAllocRastColors(obj, theDpy, theScrn, name, mapset, mode)
     XgdObject *obj;
     Display        *theDpy;
     int             theScrn;
     char           *name;
     char           *mapset;
     int             mode;
#else
XgdAllocRastColors(XgdObject *obj,
                   Display * theDpy,
		   int theScrn,
		   char *name,
		   char *mapset,
	           int mode)
#endif
{
  int index = 0;
  XColor colr;
  int ncolors, i;
  CELL cmin, cmax, cat;
  unsigned char r, g, b, junk;

  _XgdUpdateLut(theDpy, __XGDColormap, __XGDTotalCells);

  colr.flags = DoRed | DoGreen | DoBlue;
  if (!obj->Obj.GeoFrame.colorsExist) {
    if (G_read_colors(name, mapset, &obj->Obj.GeoFrame.colors) < 0) {
      XgdError("Color file not available");
    }
    obj->Obj.GeoFrame.colorsExist = True;
  }
  
  /* the number of colors = max - min + 1 + no data color */
  G_get_color_range (&cmin, &cmax, &obj->Obj.GeoFrame.colors);
  ncolors = cmax - cmin + 2;
  
  /* numcols tells us the number of colors less the no data color 
   * which is always lookup_tbl[0] 
   */
  obj->Obj.GeoFrame.numcols = ncolors - 1;

  /* 
   * If in fixed mode, just lookup the closest ones and borrow them
   */
  if ( __XGDFixedColors ) {
    for ( i = 0, cat = cmin; cat <= cmax; cat++, i++ ) {
        G_lookup_colors (&cat, &r, &g, &b, &junk, 1, &obj->Obj.GeoFrame.colors);
        colr.red = r << 8;
        colr.green = g << 8;
        colr.blue = b << 8;
        _XgdBorrowColor(&colr);
        obj->Obj.GeoFrame.lookup_tbl[i] = colr.pixel;
    }
    return;
  }
  
  /*
   * We must dither this map, build the biggest lookup we can, then assign
   * the colors.
   */
  if ( mode == XGD_DITHER_COLORS ) {
      _XgdBuildFixedColorLut(theDpy, __XGDColormap, &obj->Obj.GeoFrame.fixed,
        &obj->Obj.GeoFrame.fixedEntries);
    obj->Obj.GeoFrame.fixedLut = True;
    obj->Obj.GeoFrame.lookup_tbl = obj->Obj.GeoFrame.fixed;
    /* 
     * mark each entry in the global lut, even though we may not be "using" it 
     */
    for ( i = 0; i <=  obj->Obj.GeoFrame.fixedEntries; i++) {
        int lutIndex;
 
        if ( (lutIndex = 
            _XgdMapLookupIntoLutEntry(obj->Obj.GeoFrame.lookup_tbl[i])) > -1 ) {
            __XGDLut[lutIndex].accesses++;
        }
    }
      return;
  }

  /*
   * Can we use ALL unique colors ?
   * If we can, use them, else we need to get make a pass at matching close
   * colors for purposes of borrowing them...and then use the available ones.
   */

  
  if ( obj->Obj.GeoFrame.numcols <= __XGDPrivateCellsLeft - index ) {
    for ( i = 0, cat = cmin; cat <= cmax; cat++, i++) {
        G_lookup_colors (&cat, &r, &g, &b, &junk, 1, &obj->Obj.GeoFrame.colors);
        colr.red = r << 8;
        colr.green = g << 8;
        colr.blue = b << 8;
	if ( !_XgdGetFirstAvailableLutIndex(theDpy, __XGDColormap, &index) ) {
            XgdWarning("No available colors, results are unpredictable.");
            _XgdBorrowColor(&colr);
            obj->Obj.GeoFrame.lookup_tbl[i] = colr.pixel;
	} else {
	    colr.pixel = __XGDLut[index].pixel;
	    __XGDLut[index].status = XG_LUT_STATUS_IN_USE;
	    obj->Obj.GeoFrame.lookup_tbl[i] = colr.pixel;
	    XStoreColor(theDpy, __XGDColormap, &colr);
	    __XGDPrivateCellsLeft--;
        }
    }
    return;
  } else {
    /* we can't use all unique colors, go to fixed mode */
    _XgdBuildFixedColorLut(theDpy, __XGDColormap, &obj->Obj.GeoFrame.fixed,
        &obj->Obj.GeoFrame.fixedEntries);
    obj->Obj.GeoFrame.fixedLut = True;
    for ( i = 0, cat = cmin; cat <= cmax; cat++, i++) {
        G_lookup_colors (&cat, &r, &g, &b, &junk, 1, &obj->Obj.GeoFrame.colors);
        colr.red = r << 8;
        colr.green = g << 8;
        colr.blue = b << 8;
        _XgdBorrowColor(&colr);
        obj->Obj.GeoFrame.lookup_tbl[i] = colr.pixel;
    }
    return;
  }
}
