
/*
 * FILE: fileclose.c
 *
 * PROGRAMMER: David M. Johnson
 * 
 * FUNCTIONS:
 *
 * FileRemove()
 * ------------
 * This function will close the current edit session and return
 * the editor to XGRE_UNLOAED mode.  This function will also 
 * remove the segment file, so the user will not be able to
 * resume the edit session later.  
 *
 * FileClose() 
 * -----------
 * This function will close the current edit session and return
 * the editor to XGRE_UNLOAED mode.  The user will be able to
 * resume the edit session later because this function does not
 * remove the segment file. 
 *
 */

#include "xgre.h"

char remove_warning[] = "\
WARNING: you are about to close and remove the raster map\n\
         segment file.  If you have not used the File-Save\n\
         menu option to save this raster map, then all of\n\
         the changes that you (may) have made will be lost.\n\n\
         Are you sure that you want to close and delete the\n\
         current segment file at this time?";
 
/******************/
/*** FileRemove ***/
/******************/

void
#ifdef _NO_PROTO
FileRemove(w, cld, cad)
   Widget w;
   XtPointer cld, cad;
#else
FileRemove(Widget w, XtPointer cld, XtPointer cad)
#endif
{
if (Global.mode == XGRE_UNLOADED) return;

if (XgYesNo(Global.applShell,remove_warning))
   {
   KillRasterDialogs();
   RemoveSegmentFile(); 
   SetEditMode(XGRE_LOCKED);
   SetEditMode(XGRE_UNLOADED);
   XtUnmanageChild(Global.imageArea);
   XDestroyImage(Global.image);
   XtManageChild(Global.imageArea);
   }
}

/*****************/
/*** FileClose ***/
/*****************/

void
#ifdef _NO_PROTO
FileClose(w, cld, cad)
   Widget w;
   XtPointer cld, cad;
#else
FileClose(Widget w, XtPointer cld, XtPointer cad)
#endif
{
if (Global.mode == XGRE_UNLOADED) return;

CloseSegmentFile();

SetEditMode(XGRE_UNLOADED);
KillRasterDialogs();

/* clear image area, reset to 500x500 size */
XClearWindow(Global.display,XtWindow(Global.imageArea));
Global.imageHeight = 500;
Global.imageWidth = 500;
XtVaSetValues(Global.imageArea,XmNwidth,Global.imageWidth,
   XmNheight,Global.imageHeight,NULL);

/* free-up Ximage memory */
XDestroyImage(Global.image);
}

