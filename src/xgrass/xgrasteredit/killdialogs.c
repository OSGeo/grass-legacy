
/* 
 * FILE: killdialogs.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 *
 * KillRasterDialogs()
 * -------------------
 * This function destroys all of the dialogs that need to be
 * destroyed when the user chooses to open a new raster map 
 * for editing or resume a previous editing session.
 *
 * KillBrushDialogs()
 * ------------------
 * This function destroys all of the dialogs that need to be
 * destroyed when the user chooses to load a new brush. 
 *
 */

#include "xgre.h"

/*************************/
/*** KillRasterDialogs ***/
/*************************/

void KillRasterDialogs()
{
if (Global.FviewZoomW) XtDestroyWidget(Global.viewZoomW); 
Global.FviewZoomW = False;

if (Global.FviewIndexW) XtDestroyWidget(Global.viewIndexW); 
Global.FviewIndexW = False;

if (Global.FeditCatsD) XtDestroyWidget(Global.editCatsD); 
Global.FeditCatsD = False;

if (Global.FbrushCatD) XtDestroyWidget( Global.brushCatD); 
Global.FbrushCatD = False;
}

/************************/
/*** KillBrushDialogs ***/
/************************/

void KillBrushDialogs()
{
if (Global.FbrushCatD) XtDestroyWidget(Global.brushCatD); 
Global.FbrushCatD = False;

if (Global.FbrushColorD) XtDestroyWidget(Global.brushColorD); 
Global.FbrushColorD = False;

if (Global.FbrushEditD) XtDestroyWidget(Global.brushEditD); 
Global.FbrushEditD = False;

if (Global.FbrushCreateD) XtDestroyWidget(Global.brushCreateD); 
Global.FbrushCreateD = False;
}



