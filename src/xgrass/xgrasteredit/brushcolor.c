
/*
 * FILE: brushcolor.c 
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 *
 * BrushColor()
 * ------------
 * Callback for the Brush-Color menu option.  This function presents
 * a dialog based so that the user can change the color that is left
 * by the brush.
 *
 ********************************************************************
 * 
 * Some stuff about the Xgi library Pixel widget: 
 *
 * XmNscaleTypeMask
 * XmNcolormap
 * XmNscale1LabelString
 * XmNscale2LabelString
 * XmNscale3LabelString
 * XmNxColor
 * XmNxColorOriginal
 * XmNcancelRestore
 * 
 * #define XgRGB 0
 * #define XgHSV 1
 * #define XgCMY 2
 * 
 * extern Widget XgPixelGetChild( 
 *    Widget fs, 
 *    unsigned char which) ;
 *
 * extern Widget XgCreatePixel( 
 *    Widget p, 
 *    String name, 
 *    ArgList args,
 *    Cardinal n) ; 
 *
 * extern Widget XgCreatePixelDialog( 
 *    Widget ds_p, 
 *    String name,
 *    ArgList fsb_args, 
 *    Cardinal fsb_n) ;
 */

#include "xgre.h"

void brushColorOkCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/******************/
/*** BrushColor ***/
/******************/

int 
#ifdef _NO_PROTO
BrushColor()
#else
BrushColor()
#endif
{
   Arg al[20]; 
   int ac;
   Widget form;
   Widget pixel;
   XColor xcolor;
   XmString xms;

   if (Global.FbrushColorD) return;
   Global.FbrushColorD = True; 

/*** TOPLEVEL WIDGETS ***/

   /* interactor */
   ac = 0;
   xms = XmStringCreateSimple("Set brush highlight color");
   XtSetArg(al[ac],XmNpromptLabelString,xms); ac++;
   Global.brushColorD 
      = XgCreateInteractorDialog(Global.applShell,"BrushColor",al,ac);
   XtManageChild(
      XgInteractorGetChild(Global.brushColorD,XmINTERACT_PROMPT_LABEL));
   XtAddCallback(Global.brushColorD,XmNokCallback,brushColorOkCB,NULL);
   XtAddCallback(Global.brushColorD,XmNcancelCallback,brushColorOkCB,NULL);
   form = XtVaCreateManagedWidget("form",xmFormWidgetClass,
      Global.brushColorD,NULL);

/*** COLOR-EDITOR WIDGET ***/ 

   /* get highlight pixel color */
   xcolor.pixel = (Pixel)Global.highlight; 
   XQueryColor(Global.display, Global.cmap, &xcolor);

   /* setup the "pixel" one-color color-editor widget */ 
   ac = 0;
   XtSetArg(al[ac],XmNcolormap,Global.cmap); ac++;
   XtSetArg(al[ac],XmNxColor,&xcolor); ac++;
   XtSetArg(al[ac],XmNleftAttachment,XmATTACH_FORM); ac++;
   XtSetArg(al[ac],XmNrightAttachment,XmATTACH_FORM); ac++;
   XtSetArg(al[ac],XmNbottomAttachment,XmATTACH_FORM); ac++;
   XtSetArg(al[ac],XmNtopAttachment,XmATTACH_FORM); ac++;
   pixel = XgCreatePixel(form,"pixel",al,ac);
   XtUnmanageChild(XgInteractorGetChild(pixel,XmINTERACT_OK_BUTTON));
   XtUnmanageChild(XgInteractorGetChild(pixel,XmINTERACT_CANCEL_BUTTON));
   XtUnmanageChild(XgInteractorGetChild(pixel,XmINTERACT_HELP_BUTTON));

   XtManageChild(pixel);
   XtManageChild(Global.brushColorD);
}

/**********************/
/*** brushColorOkCB ***/
/**********************/

void 
#ifdef _NO_PROTO
brushColorOkCB(w,cld,cad)
   Widget w;
   XtPointer cld;
   XtPointer cad;
#else
brushColorCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Global.FbrushColorD = False;
XtDestroyWidget(w);
}

