#include "xgrass_dlib.h"
#include "clrtbl.h"

void
#ifdef _NO_PROTO
_XgdFreeColors(obj)
     XgdObject *obj;
#else
_XgdFreeColors(XgdObject *obj)
#endif
{
  Pixel  *l;
  Pixel  *fl;
  int     nclrs;
  int     i, lutIndex;
  XColor black;

  black.red = 0; black.green = 0; black.blue = 0;
  black.flags = DoRed | DoGreen | DoBlue;

  l = obj->Obj.GeoFrame.lookup_tbl;
  nclrs = obj->Obj.GeoFrame.numcols;

  if ( nclrs == 0 ) return;

  if ( obj->Obj.GeoFrame.fixedLut ) {
      fl = obj->Obj.GeoFrame.fixed;
      if ( obj->Obj.GeoFrame.colormode == XGD_DITHERED_IMAGE ) {
	  /* 
	   * decrement the accesses counter for every entry in the fixed 
           * lookup table since we don't borrow colors and we assume an access
           * count of 1 for entries in the dithered image. This will not affect
           * the fact that other maps might be borrowing one of these entries.
	   */
	  for (i = 0; i < obj->Obj.GeoFrame.fixedEntries; i++) {
	      if ( (lutIndex = _XgdMapLookupIntoLutEntry(fl[i])) > -1 ) {
		      __XGDLut[lutIndex].accesses--;
	      }
	  }
	  /* 
	   * now, if we have no more accesses, free the color
	   */
	  for (i = 0; i < obj->Obj.GeoFrame.fixedEntries; i++) {
	      if ( (lutIndex = _XgdMapLookupIntoLutEntry(fl[i])) > -1 ) {
		  if ( !__XGDFixedColors && __XGDLut[lutIndex].accesses < 1 &&
                       __XGDLut[lutIndex].status != XG_LUT_STATUS_NO_FREE ) {
		      black.pixel = __XGDLut[lutIndex].pixel;
		      XStoreColor(obj->display, __XGDColormap, &black);
		      __XGDPrivateCellsLeft++;
		      __XGDLut[lutIndex].status = XG_LUT_STATUS_AVAILABLE;
                      XFreeColors(obj->display, __XGDColormap, 
                          &(__XGDLut[lutIndex].pixel), 1, 0);
		  }
	      }
	  }
      } else {
	  /* 
	   * decrement the accesses counter in the lookup table
	   * so we can determine if other maps are borrowing any of the entries 
	   */
	  for (i = 0; i <= nclrs; i++) {
	      if ( (lutIndex = _XgdMapLookupIntoLutEntry(l[i])) > -1 ) {
		      __XGDLut[lutIndex].accesses--;
	      }
	  }
	  /* 
	   * now, if we have no more accesses, free the colors borrowed and
           * the fixed color table.
	   */
	  for (i = 0; i <= nclrs; i++) {
	      if ( (lutIndex = _XgdMapLookupIntoLutEntry(l[i])) > -1 ) {
		  if ( !__XGDFixedColors && __XGDLut[lutIndex].accesses < 1 &&
                       __XGDLut[lutIndex].mode == XG_LUT_MODE_RW &&
                       __XGDLut[lutIndex].status != XG_LUT_STATUS_NO_FREE ) {
		      black.pixel = __XGDLut[lutIndex].pixel;
		      XStoreColor(obj->display, __XGDColormap, &black);
		      __XGDPrivateCellsLeft++;
		      __XGDLut[lutIndex].status = XG_LUT_STATUS_AVAILABLE;
                      XFreeColors(obj->display, __XGDColormap, 
                          &(__XGDLut[lutIndex].pixel), 1, 0);
		  }
	      }
	  }
	  for (i = 0; i < obj->Obj.GeoFrame.fixedEntries; i++) {
	      if ( (lutIndex = _XgdMapLookupIntoLutEntry(fl[i])) > -1 ) {
		  if ( !__XGDFixedColors && __XGDLut[lutIndex].accesses < 1 &&
                       __XGDLut[lutIndex].status != XG_LUT_STATUS_NO_FREE ) {
		      black.pixel = __XGDLut[lutIndex].pixel;
		      XStoreColor(obj->display, __XGDColormap, &black);
		      __XGDPrivateCellsLeft++;
		      __XGDLut[lutIndex].status = XG_LUT_STATUS_AVAILABLE;
                      XFreeColors(obj->display, __XGDColormap, 
                          &(__XGDLut[lutIndex].pixel), 1, 0);
		  }
	      }
	  }
      }
      XtFree(obj->Obj.GeoFrame.fixed);
      obj->Obj.GeoFrame.fixedEntries = 0;
      obj->Obj.GeoFrame.fixedLut = NULL;
  } else {
      /* 
       * we're not in fixed mode or dithering, so just determine whether
       * we own the cell, if we do and the cell isn't being accessed by 
       * any other map, then free the color cell and reset the status.
       */
      for (i = 0; i <= nclrs; i++) {
	  if ( (lutIndex = _XgdMapLookupIntoLutEntry(l[i])) > -1 ) {
	      if ( __XGDLut[lutIndex].mode == XG_LUT_MODE_RW ) {
		  if ( __XGDLut[lutIndex].accesses < 2 &&
                       __XGDLut[lutIndex].status != XG_LUT_STATUS_NO_FREE ) {
		      black.pixel = __XGDLut[lutIndex].pixel;
		      XStoreColor(obj->display, __XGDColormap, &black);
		      __XGDPrivateCellsLeft++;
                      XFreeColors(obj->display, __XGDColormap, 
                          &(__XGDLut[lutIndex].pixel), 1, 0);
		  }         
		  switch ( __XGDLut[lutIndex].status ) {
		      case XG_LUT_STATUS_FIXED:
		      case XG_LUT_STATUS_IN_USE:
			  __XGDLut[lutIndex].status = XG_LUT_STATUS_AVAILABLE;
			  break;
		      case XG_LUT_STATUS_SHARED:
			  __XGDLut[lutIndex].status = XG_LUT_STATUS_IN_USE;
			  break;
		  }
		  __XGDLut[lutIndex].accesses--;
	      }
	  }
      }
  }
  /* make sure this flag is unset */
  if ( obj->Obj.GeoFrame.colorsExist ) 
      obj->Obj.GeoFrame.colorsExist = False;
  obj->Obj.GeoFrame.lookup_tbl = NULL;
  obj->Obj.GeoFrame.numcols = 0;
}
