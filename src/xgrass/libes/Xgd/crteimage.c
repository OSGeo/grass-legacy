#include "xgrass_dlib.h"
#include "gis.h"
#include "hcolr.h"


#define FIXEDCOLR	12

Pixel *lookup_tbl;
Pixel *vect_tbl;

int ncolors;

Pixel s_cidx;
Pixel e_cidx;
Pixel c_cidx;



Pixel plane_masks[2];

#define NO_DITHER               1
#define DITHER                  2


/*
 ***************************************************************************
 * XgdCreateImage - Create the image, figuring out what type of image to
 *   create: Dithered, no dithered, b&w, etc.
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdCreateImage(obj, scrn,  name, mapset, mode)
     XgdObject *obj;
     int       scrn;
     char      *name;
     char       *mapset;
     int             mode;
#else
XgdCreateImage(XgdObject *obj,
	       int scrn,
	       char *name,
	       char *mapset,
	       int mode)
#endif
{
  
  if (!obj->Obj.GeoFrame.colorsExist){
    if (G_read_colors(name, mapset, &obj->Obj.GeoFrame.colors) < 0)
      XgdError("Color file not available");

  
    ncolors = obj->Obj.GeoFrame.colors.fixed.max - obj->Obj.GeoFrame.colors.fixed.min + 1;
  
    if (ncolors < 256 && mode == NO_DITHER ) {
      if (!(obj->Obj.GeoFrame.lookup_tbl = (Pixel *)
	    calloc(ncolors + 2, sizeof(Pixel))))
	XgdError("Can't allocate color table");
      if (!(obj->Obj.GeoFrame.borrowed = (Pixel *)
	    calloc(ncolors + 2, sizeof(Pixel))))
	XgdError("can't allocate color table");
    } else {
      if (!(obj->Obj.GeoFrame.lookup_tbl = (Pixel *)
	    calloc(300, sizeof(u_long))))
	XgdError("can't allocate color table");
      if (!(obj->Obj.GeoFrame.borrowed = (Pixel *)
	    calloc(300, sizeof(Pixel))))
	XgdError("can't allocate color table");
    }
  }
  
  switch ((int) mode) {
  case NO_DITHER:
    if (ncolors < 256) {
      
      if (__XGDVInfo->depth == 0) {
	XgdMonoColor(obj, name, mapset, obj->Obj.GeoFrame.lookup_tbl);
	XgdBWImage(obj, scrn, name, mapset);
	hgdither = NO_DITHER;
	obj->Obj.GeoFrame.colormode = XGD_STANDARD_IMAGE;
	
      } else if (__XGDVInfo->visual->class == PseudoColor ||
		 __XGDVInfo->visual->class == TrueColor ||
		 __XGDVInfo->visual->class == DirectColor) {

	if (!obj->Obj.GeoFrame.colorsExist){
	  _XgdFreeColors(obj);
	
	  XgdAllocRastColors(obj, obj->display, scrn,
			     name, mapset, 0, NO_DITHER);
	  obj->Obj.GeoFrame.colorsExist = True;
	}
	XgdRasterImage(obj, scrn);
	
	hgdither = NO_DITHER;
	obj->Obj.GeoFrame.colormode = XGD_STANDARD_IMAGE;
      } else if (__XGDVInfo->visual->class == GrayScale ||
		 __XGDVInfo->visual->class == StaticColor) {

	if (!obj->Obj.GeoFrame.colorsExist){
	  _XgdFreeColors(obj);
	
	  XgdAllocRastColors(obj, obj->display, scrn, 
			     name, mapset, XGD_NO_DITHER);
	  obj->Obj.GeoFrame.colorsExist = True;
	}
	XgdRasterImage(obj, scrn);
	hgdither = NO_DITHER;
	obj->Obj.GeoFrame.colormode = XGD_STANDARD_IMAGE;
      }
    } else {
      if (__XGDVInfo->depth == 0) {
	XgdMonoColor(obj, name, mapset, obj->Obj.GeoFrame.lookup_tbl);
	XgdBWImage(obj, scrn, name, mapset);
	obj->Obj.GeoFrame.colormode = XGD_STANDARD_IMAGE;
      } else if (__XGDVInfo->visual->class == PseudoColor ||
		 __XGDVInfo->visual->class == TrueColor ||
		 __XGDVInfo->visual->class == DirectColor) {

	if (!obj->Obj.GeoFrame.colorsExist){
	  XgdAllocRastColors(obj, obj->display, scrn, name,
			     mapset, XGD_DITHER_COLORS);
	}
        XgdDitherImage(obj, scrn, name, mapset, -1, -1);
	obj->Obj.GeoFrame.colormode = XGD_DITHERED_IMAGE;
	hgdither = DITHER;
      } else if (__XGDVInfo->visual->class == GrayScale ||
		 __XGDVInfo->visual->class == StaticColor) {
	if (!obj->Obj.GeoFrame.colorsExist){
	    XgdAllocRastColors(obj, obj->display, scrn, name, mapset, 
		XGD_DITHER_COLORS);
	}
	XgdDitherImage(obj, scrn, name, mapset, -1, -1);
	obj->Obj.GeoFrame.colormode = XGD_DITHERED_IMAGE;
	hgdither = DITHER;
	ncolors = 256;
      }
    }
    break;
    
  case DITHER:
    {
      
      if (__XGDVInfo->depth == 0) {
	XgdMonoColor(obj, name, mapset, obj->Obj.GeoFrame.lookup_tbl);
	XgdBWImage(obj, scrn, name, mapset,
					obj->Obj.GeoFrame.lookup_tbl);
	hgdither = DITHER;
      } else if (__XGDVInfo->visual->class == PseudoColor ||
		 __XGDVInfo->visual->class == TrueColor ||
		 __XGDVInfo->visual->class == DirectColor) {
	XgdAllocRastColors(obj, obj->display, scrn, 
			   name, mapset, XGD_DITHER_COLORS);
	XgdDitherImage(obj, scrn, name, mapset, -1, -1);
	obj->Obj.GeoFrame.colormode = XGD_DITHERED_IMAGE;
	hgdither = DITHER;
	ncolors = 256;
      } else if (__XGDVInfo->visual->class == GrayScale ||
		 __XGDVInfo->visual->class == StaticColor) {
	XgdAllocRastColors(obj, obj->display, scrn, 
			   name, mapset, XGD_DITHER_COLORS);
	XgdDitherImage(obj, scrn, name, mapset,
					    -1, -1);
	obj->Obj.GeoFrame.colormode = XGD_DITHERED_IMAGE;
	hgdither = DITHER;
	ncolors = 256;
      }
      break;
    }
    
  }
}
