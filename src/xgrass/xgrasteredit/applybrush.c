
/*
 * FILE: applybrush.c
 *
 * FUNCTIONS:
 *
 * PROGRAMMER: David M. Johnson
 *
 * ApplyBrushCell(bx,by,sx,sy)
 * ---------------------------
 * Applies brush cell (bx,by) of the brush (Global.brush) to the 
 * raster map cell at (sx,sy).  If the raster map cell value is 
 * changed then the old value is saved in the undo buffer, and the 
 * new value is figured into the range (Global.range) and color-table 
 * (Global.colors).
 *
 * ApplyBrush(ax,ay)
 * -----------------
 * Applies the brush (Global.brush) to raster map, positioning the 
 * brush's hot-spot cell on the raster map cell at (ax,ay).  Call the
 * function ApplyBrushCell for each brush cell.
 *
 */

#include "xgre.h"

int ApplyBrushCell(
#ifndef _NO_PROTO
   int bx,
   int by,
   int sx,
   int sy
#endif
);

/******************/
/*** ApplyBrush ***/
/******************/

int 
#ifdef _NO_PROTO
ApplyBrush(ax,ay)
   int ax,ay;       /* segment coords of brush application point */
#else
ApplyBrush(int ax, int ay)
#endif
{
int cx,cy;   /* segment coords of upper-left corner of brush */
int bx,by;   /* brush cell coords */

/* figure upper left corner of brush */
cx = ax - Global.brushHotCol; 
cy = ay - Global.brushHotRow; 

/* loop through rows of brush cells */
for (by=0; by < Global.brushRows; by++)
   {
   /* loop through cols of brush cells */
   for (bx=0; bx < Global.brushCols; bx++)
      {
      /* apply brush cell to segment file cell */ 
      if (ApplyBrushCell(bx,by,cx+bx,cy+by) == 0)
         {
         /* update main image pixel */
         HighlightImagePixel(cx+bx,cy+by);

         /* update zoom image pixel */
         if (Global.zoomLoaded) HighlightZoomPixel(cx+bx,cy+by);
         }
      }
   }
return(0);
}

/**********************/
/*** ApplyBrushCell ***/
/**********************/

int 
#ifdef _NO_PROTO
ApplyBrushCell(bx,by,sx,sy)
   int bx,by;                 /* brush cell to apply */
   int sx,sy;                 /* segment cell to apply it to */ 
#else
ApplyBrushCell(int bx, int by, int sx, int sy)
#endif
{
CELL inputval, cellval, newval, min, max;

if (sx>Global.seghd.cols || sx<0 || sy>Global.seghd.rows || sy<0) 
   return(1);

if (!segment_get(&(Global.seg),(char*)&cellval,sy,sx))
   {
   XgError(Global.applShell,"Error reading from segment file");
   return(-1);
   }

/*** DETERMINE THE INPUT VALUE ***/

switch (Global.brush[bx][by].input)
   {
   case XGRE_INPCAT: 
      /* use the brush category value */
      inputval = Global.brushCat;
      break;
   case XGRE_INPVALUE:
      /* use the brush cell value */
      inputval = Global.brush[bx][by].value; 
      break;
   case XGRE_INPREAD: 
      /* use the segment cell value directly under the brush cell */ 
      inputval = cellval;
      break;
   }

/*** PERFORM THE OPERATION ***/

switch (Global.brush[bx][by].op)
   {
   case XGRE_OPCOPY: 
      /* just use the input value */
      newval = inputval;
      break;
   case XGRE_OPERASE: 
      /* add the input value to the segment-cell value */
      newval = 0;
      break;
   case XGRE_OPADD: 
      /* add the input value to the segment-cell value */
      newval = cellval + inputval;
      break;
   case XGRE_OPMULT: 
      /* multiply the segment-cell value by the input value */
      newval = cellval * inputval;
      break;
   case XGRE_OPCALC: 
      /* future enhancement: mapcalc style operation */
      return(1);
   case XGRE_OPNULL: 
      /* do nothing */ 
      return(1);
   }

/*** UPDATE THE SEGMENT FILE ***/

if (!segment_put(&(Global.seg),(char*)&newval,sy,sx))
   {
   XgError(Global.applShell,"Error writing to segment file");
   return(-1);
   }
AddToUndoBuffer(sy,sx,cellval);

/* factor new cell value into the range */
G_update_range(newval,&(Global.range)); 

/* factor new range into color-table */ 
min = Global.range.nmin ? Global.range.nmin : Global.range.pmin;
max = Global.range.pmax ? Global.range.pmax : Global.range.nmax;
G_set_color_range(min,max,&(Global.colors));

/* update the modification bounds */
if (sx > Global.modX2) Global.modX2 = sx;
if (sx < Global.modX1) Global.modX1 = sx;
if (sy > Global.modY2) Global.modY2 = sy;
if (sy < Global.modY1) Global.modY1 = sy;

return(0);
}



