
/*
 * FILE: brushcat.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 *
 * BrushCat()
 * ----------
 * Callback function for the Brush-Category menu option.  This 
 * function presents an Interactor List Dialog box so that the 
 * user may choose a raster map category value to be associated 
 * with the brush. 
 *
 * CatOkCB()
 * ---------
 * Callback function for the Interactor List Dialog OK button.  
 * This function stores the user's choice in Global.brushCat.
 *
 */

#include "Interact.h"
#include "xgre.h"

void catOkCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

void brushCatCancelCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/****************/
/*** BrushCat ***/
/****************/

void
#ifdef _NO_PROTO
BrushCat(w, cld, cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else
BrushCat(Widget w, XtPointer cld, XtPointer cad)
#endif
{
   Arg al[15];
   int ac = 0;
   int i; 
   int numItems;
   char buf[2048]; /* ...because Michael said so... */
   char *label;
   CELL min, max;
   char *cstr;
   XmString cxms, lxms, txms, pxms;
   XmStringTable catList;

   if (Global.FbrushCatD) return;
   Global.FbrushCatD = True; 

   if (Global.mode == XGRE_UNLOADED)
      {
      XgError(Global.applShell,
         "No raster map loaded, cannot set brush-category yet");
      return;
      }

/*** FILL STRING-TABLE WITH RASTER MAP CATEORIES ***/

   G_get_range_min_max(&(Global.range), &min, &max);
   /* FIX: is there a better way to allow user to access category zero? */
   if (min > 0) min=0;
   if (max < 0) max=0;

   numItems = max - min + 1;
   catList = (XmStringTable)XtCalloc(numItems,sizeof(XmString));
   for (i = min; i <= max; i++) 
      {
      label = G_get_cat((CELL) i,&(Global.cats));
      if (*label == NULL) 
         sprintf(buf, "%d: no-label", i);
      else 
         sprintf(buf, "%d: %s", i, label);
      catList[i-min] = XmStringCreateSimple(buf);
      }

/*** CREATE INTERACTOR LIST DIALOG ***/

   /* create label strings for dialog */
   label = G_get_cat((CELL)Global.brushCat,&(Global.cats));
   if (*label == NULL)
      sprintf(buf,"%d: no-label",Global.brushCat);
   else
      sprintf(buf,"%d: %s",Global.brushCat,label);
   catList[i-min] = XmStringCreateSimple(buf);
   cxms= XmStringCreateSimple(buf);
   pxms= XmStringCreateSimple("Select brush category:");
   txms= XmStringCreateSimple("Selected category:");
   lxms= XmStringCreateSimple("Categories:");

   /* create the dialog itself */
   XtSetArg(al[ac],XmNpromptLabelString,pxms); ac++;
   XtSetArg(al[ac],XmNlistTextString,cxms); ac++;
   XtSetArg(al[ac],XmNlistLabelString,lxms); ac++;
   XtSetArg(al[ac],XmNlistTextLabelString,txms); ac++;
   XtSetArg(al[ac],XmNlistItemCount,numItems); ac++;
   XtSetArg(al[ac],XmNlistItems,catList); ac++;
   XtSetArg(al[ac],XmNmustMatch,True); ac++;
   Global.brushCatD = XgCreateInteractorListDialog(Global.applShell,
      "BrushCat",al,ac);
   XtManageChild(XgInteractorGetChild(Global.brushCatD,
      XmINTERACT_PROMPT_LABEL));
   XtAddCallback(Global.brushCatD,XmNokCallback,catOkCB,cld);
   XtAddCallback(Global.brushCatD,XmNcancelCallback,brushCatCancelCB,NULL);
   XgAddHelpCallBackFromFile(Global.brushCatD,"xgre/xgre_cat_list");
   XtManageChild(Global.brushCatD);
}

/***************/
/*** catOkCB ***/
/***************/

void
#ifdef _NO_PROTO
catOkCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
catOkCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
   char *result;
   char *p1, *p2;
   XmString xms;

   Global.FbrushCatD = False;

   XtVaGetValues(Global.brushCatD, XmNlistTextString, &xms, NULL);
   XmStringGetLtoR(xms,XmSTRING_DEFAULT_CHARSET,&result);

#  ifdef DEBUG
   printf("result=%s\n",result);
#  endif

   if ( result == NULL ) return;

   p1 = (char*)strtok(result,":"); p2 = (char*)0;
   if (sscanf(p1,"%ld",(CELL*)(&(Global.brushCat))) != 1)
      {
      XgError(Global.applShell,"Error getting category from list");
      return;
      }
}

/************************/
/*** brushCatCancelCB ***/
/************************/

void
#ifdef _NO_PROTO
brushCatCancelCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
brushCatCancelCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Global.FbrushCatD = False;
XtDestroyWidget(w);
}


