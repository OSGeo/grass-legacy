
/*
 * FILE: editcats.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 *
 * EditCats()
 * ----------
 * This function sets up a dialog using the XGRASS cats 
 * widget so that the user may edit the current global cats 
 * structure (Global.cats)
 *
 * catsOkCB()
 * ----------
 * OK button callback function.  Resets the global range 
 * range (Global.range) and colors structure (Global.colors). 
 *
 */

#include "Cats.h"
#include "xgre.h"

/*** LOCAL PROTOTYPES ***/

void catsOkCB(
#ifndef _NO_PROTO
   Widget    w,
   XtPointer cli,
   XtPointer call
#endif
);

void editCatsCancelCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/****************/
/*** EditCats ***/
/****************/

void
#ifdef _NO_PROTO
EditCats(w, cld, cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else
EditCats(Widget w, XtPointer cld, XtPointer cad)
#endif
{
   Arg al[15];
   int ac = 0;

   if (Global.FeditCatsD) return;
   Global.FeditCatsD = True;

   if (Global.mode == XGRE_UNLOADED) 
      {
      XgError(Global.applShell,
         "No raster map loaded, cannot edit categories yet");
      return;
      }

   if (Global.mode == XGRE_LOCKED) 
      {
      XgError(Global.applShell,
         "Editor is locked, cannot edit categories");
      return;
      }

/*** CREATE CATEGORY EDITOR DIALOG ***/

   XtSetArg(al[ac],XmNcats,&(Global.cats)); ac++;
   XtSetArg(al[ac],XmNrange,&(Global.range)); ac++;
   Global.editCatsD = XgCreateCatsDialog(Global.applShell,"EditCats",al,ac);
   XtManageChild(
      XgInteractorGetChild(Global.editCatsD,XmINTERACT_PROMPT_LABEL));
   XtAddCallback(Global.editCatsD,XmNcancelCallback,CancelCB,NULL);
   XtManageChild(Global.editCatsD);
}

void
#ifdef _NO_PROTO
catsOkCB(w, cld, cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else
catsOkCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
/* factor new range into color-table */
CELL min = Global.range.nmin ? Global.range.nmin : Global.range.pmin;
CELL max = Global.range.pmax ? Global.range.pmax : Global.range.nmax;
G_set_color_range(min,max,&(Global.colors));

Global.FeditCatsD = False;
}

/************************/
/*** editCatsCancelCB ***/
/************************/

void
#ifdef _NO_PROTO
editCatsCancelCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
editCatsCancelCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Global.FeditCatsD = False;
XtDestroyWidget(w);
}


