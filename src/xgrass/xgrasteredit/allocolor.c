
/* 
 * FILE: allocolors.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * AllocColors(XgdObject *obj)
 * ---------------------------
 * Allocates colors for a raster map.  This function 
 * is used in place of the XGRASS Display Library function 
 * XgdAllocRastColors.  This function creates a lookup table
 * (Obj.GeoFrame.lookup_tbl) for the raster-editor's GRASS 
 * colortable (Global.colors).  This lookup table will map 
 * colortable colors to screen pixel values.
 *
 */

#include "xgre.h"

/*******************/
/*** AllocColors ***/
/*******************/

int
#ifdef _NO_PROTO
AllocColors(obj)
   XgdObject *obj;
#else
AllocColors(XgdObject *obj)
#endif
{
   int index;
   XColor colr;
   int ncolors, ncols, i, unique;
   CELL cmin, cmax, cat;
   unsigned char r, g, b, junk;

   colr.flags = DoRed | DoGreen | DoBlue;
   obj->Obj.GeoFrame.colorsExist = True;
  
   /* the number of colors = max - min + 1 + no data color */
   G_get_color_range (&cmin, &cmax,&(Global.colors));
   /* FIX: is there a better way to make sure that 0 gets a color? */
   if (cmin > 0) cmin = 0; 
   if (cmax < 0) cmax = 0;
   ncolors = cmax - cmin + 2;
#  ifdef DEBUG
   printf("AllocColors: cmin=%ld cmax=%ld\n",cmin,cmax);
#  endif

   /* allocate space for the lookup table and borrowed colors
    * (from Xgd library function in crteimage.c)
    */
   if (!(obj->Obj.GeoFrame.lookup_tbl = (Pixel *)
      calloc(ncolors + 2, sizeof(Pixel))))
      XgdError("Can't allocate color table");
   if (!(obj->Obj.GeoFrame.borrowed = (Pixel *)
      calloc(ncolors + 2, sizeof(Pixel))))
      XgdError("can't allocate color table");

   /* numcols tells us the number of colors less the no data color 
    * which is always lookup_tbl[0] 
    */
   obj->Obj.GeoFrame.numcols = ncolors - 1;
   ncols = ncolors - 1;

   /* 
    * If in fixed mode, just lookup the closest ones and borrow them
    */
   for (i=0, cat=cmin; cat <= cmax; cat++, i++ ) 
      {
      G_lookup_colors(&cat,&r,&g,&b,&junk,1,&(Global.colors));
      colr.red = r << 8;
      colr.green = g << 8;
      colr.blue = b << 8;
      /* FIX don`t borrow the highlight color */ 
      _XgdBorrowColor(&colr);
      obj->Obj.GeoFrame.lookup_tbl[i] = colr.pixel;
      }

   return;
}


