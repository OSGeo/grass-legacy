
/* 
 * FILE: editcb.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 * 
 * EditLockCB()
 * ------------
 * This callback function changes the editor's mode from 
 * XGRE_LOCKED to XGRE_NORMAL or vice-versa.
 *
 * EditVerticalCB()
 * ----------------
 * This callback sets the editor's mode to XGRE_VERTI_DRAG.
 * If the mode is already set to XGRE_VERTI_DRAG then this 
 * function changes the mode to XGRE_NORMAL.  It will 
 * present an error dialog if the mode is XGRE_LOCKED. 
 *
 * EditHorizontalCB()
 * ------------------
 * This callback sets the editor's mode to XGRE_HORIZ_DRAG.
 * If the mode is already set to XGRE_HORIZ_DRAG then this 
 * function changes the mode to XGRE_NORMAL.  It will 
 * present an error dialog if the mode is XGRE_LOCKED.
 *
 * EditPolygonRegionCB()
 * ---------------------
 * This callback sets the editor's mode to XGRE_POLY_EDIT.
 * If the mode is already set to XGRE_POLY_EDIT then this 
 * function changes the mode to XGRE_NORMAL.  It will 
 * present an error dialog if the mode is XGRE_LOCKED or
 * if the brush is not a 1x1 brush. 
 *
 * EditBoxRegionCB()
 * -----------------
 * This callback sets the editor's mode to XGRE_BOX_EDIT.
 * If the mode is already set to XGRE_BOX_EDIT then this 
 * function changes the mode to XGRE_NORMAL.  It will 
 * present an error dialog if the mode is XGRE_LOCKED or
 * if the brush is not a 1x1 brush.
 *
 * EditUndoCB()
 * ------------
 * This callback calls the EditUndo() function.
 *
 */ 

#include "xgre.h"

/******************/
/*** EditLockCB ***/
/******************/
 
void
#ifdef _NO_PROTO
EditLockCB(w, cld, cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else
EditLockCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
if (Global.mode == XGRE_UNLOADED)
   {
   XgError(Global.applShell,"No raster map loaded, cannot unlock");
   return;
   }
else if (Global.mode == XGRE_LOCKED)
   SetEditMode(XGRE_NORMAL); 
else
   SetEditMode(XGRE_LOCKED);
}

/**********************/
/*** EditVerticalCB ***/
/**********************/
 
void
#ifdef _NO_PROTO
EditVerticalCB(w, cld, cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else
EditVerticalCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
if (Global.mode == XGRE_UNLOADED)
   {
   XgError(Global.applShell,"No raster map loaded, cannot edit");
   return;
   }
else if (Global.mode == XGRE_LOCKED)
   {
   XgError(Global.applShell,"Editor is locked, cannot edit");
   return;
   }
else if (Global.mode == XGRE_VERTI_DRAG)
   SetEditMode(XGRE_NORMAL);
else 
   SetEditMode(XGRE_VERTI_DRAG);
}

/************************/
/*** EditHorizontalCB ***/
/************************/
 
void
#ifdef _NO_PROTO
EditHorizontalCB(w, cld, cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else
EditHorizontalCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
if (Global.mode == XGRE_UNLOADED)
   {
   XgError(Global.applShell,"No raster map loaded, cannot edit");
   return;
   }
else if (Global.mode == XGRE_LOCKED)
   {
   XgError(Global.applShell,"Editor is locked, cannot edit");
   return;
   }
else if (Global.mode == XGRE_HORIZ_DRAG)
   SetEditMode(XGRE_NORMAL);
else
   SetEditMode(XGRE_HORIZ_DRAG);
}

/***********************/
/*** EditBoxRegionCB ***/
/***********************/
 
void
#ifdef _NO_PROTO
EditBoxRegionCB(w, cld, cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else
EditBoxRegionCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
if (Global.mode == XGRE_UNLOADED)
   {
   XgError(Global.applShell,"No raster map loaded, cannot edit");
   return;
   }
else if (Global.mode == XGRE_LOCKED)
   {
   XgError(Global.applShell,"Editor is locked, cannot edit");
   return;
   }
else if (!(Global.brushRows == 1 && Global.brushCols == 1))
   {
   XgError(Global.applShell,
      "Box-Region edits may only be done with a 1x1 brush");
   return;
   }
else if (Global.mode == XGRE_BOX_EDIT)
   SetEditMode(XGRE_NORMAL);
else
   SetEditMode(XGRE_BOX_EDIT);
}

/***************************/
/*** EditPolygonRegionCB ***/
/***************************/

void
#ifdef _NO_PROTO
EditPolygonRegionCB(w, cld, cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else
EditPolygonRegionCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
if (Global.mode == XGRE_UNLOADED)
   {
   XgError(Global.applShell,"No raster map loaded, cannot edit");
   return;
   }
else if (Global.mode == XGRE_LOCKED)
   {
   XgError(Global.applShell,"Editor is locked, cannot edit");
   return;
   }
else if (!(Global.brushRows == 1 && Global.brushCols == 1))
   {
   XgError(Global.applShell,
      "Polygon-Region edits may only be done with a 1x1 brush");
   return;
   }
else if (Global.mode == XGRE_POLY_EDIT)
   SetEditMode(XGRE_NORMAL);
else
   SetEditMode(XGRE_POLY_EDIT);
}

/*****************/
/*** EditUndoCB **/
/*****************/

void
#ifdef _NO_PROTO
EditUndoCB(w, cld, cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else
EditUndoCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
EditUndo();
}

