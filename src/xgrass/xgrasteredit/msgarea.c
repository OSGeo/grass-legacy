
/* 
 * FILE: msgarea.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 *
 * CreateMessageArea(Widget shell)
 * -------------------------------
 * Creates the widgets and the layout for the raster editor's
 * message area.  The message area is used to display the 
 * (row,col) coordinates of the cursor as it moves over 
 * the raster, the (easting,northing) coordinates, and the
 * category number and attribute value of the cell over
 * which the cursor is postioned.  The following global
 * text-field widgets are created by this function.  These 
 * text-field widgets are updated by the functions 
 * UpdatePostion(), UpdateCategoryText() and 
 * UpdateStatusText() in updatemsg.c.
 *
 *   Global.modeText
 *   Global.brushText
 *   Global.rowText
 *   Global.colText
 *   Global.eastText
 *   Global.northText
 *   Global.catText
 *   Global.attText
 *   
 */

#include "xgre.h"

/*************************/
/*** CreateMessageArea ***/
/*************************/

Widget 
#ifdef _NO_PROTO
CreateMessageArea(shell)
   Widget shell;
#else
   CreateMessageArea(Widget shell)
#endif
{
   Widget mainRC;
   Widget modeRC;
   Widget statusRC;       
   Widget   rowcolFrame;
   Widget   rowcolRC;
   Widget     rowRC;
   Widget     rowTextLabel;
   Widget     colRC;
   Widget     colTextLabel;
   Widget   coordFrame;
   Widget   coordRC;
   Widget     eastRC;
   Widget     eastTextLabel;
   Widget     northRC;
   Widget     northTextLabel;
   Widget   catattFrame;
   Widget   catattRC;
   Widget     catRC;
   Widget     catTextLabel;
   Widget     attRC;
   Widget     attTextLabel;

   XmString xms;
   XmString xxms;
   XmString yxms;


/*** MAIN ROW-COLUMN WIDGET ***/

   mainRC = XtVaCreateManagedWidget("message_area",
      xmRowColumnWidgetClass,shell,
      XmNorientation,XmVERTICAL,NULL);

/*** MODE ROW-COLUMN WIDGET ***/

   modeRC = XtVaCreateManagedWidget("mode_area",
      xmRowColumnWidgetClass,mainRC,
      XmNorientation,XmHORIZONTAL,NULL);

/*** MODE TEXT FIELD WIDGET ***/

   Global.modeText = XtVaCreateManagedWidget("mode_text",
       xmTextFieldWidgetClass,modeRC,
       XmNcolumns,30,XmNeditable,False,NULL);

/*** BRUSH TEXT FIELD WIDGET ***/

   Global.brushText = XtVaCreateManagedWidget("brush_text",
       xmTextFieldWidgetClass,modeRC,
       XmNcolumns,30,XmNeditable,False,NULL);

/*** STATUS ROW-COLUMN WIDGET***/

   statusRC = XtVaCreateManagedWidget("status_area",
      xmRowColumnWidgetClass, mainRC,
      XmNorientation,XmHORIZONTAL,NULL);

/*** COLUMN AND ROW TEXT FIELDS ***/

   rowcolFrame = XtVaCreateManagedWidget("rowcol_frame",
      xmFrameWidgetClass,statusRC,NULL);

   rowcolRC = XtVaCreateManagedWidget("rowcol_area",
      xmRowColumnWidgetClass,rowcolFrame,
      XmNorientation,XmVERTICAL,NULL); 
   xxms = XmStringCreateSimple("Column:");
   yxms = XmStringCreateSimple("   Row:");

   colRC = XtVaCreateManagedWidget("col_area",
      xmRowColumnWidgetClass,rowcolRC,
      XmNorientation,XmHORIZONTAL,NULL);
   colTextLabel = XtVaCreateManagedWidget("col",
       xmLabelWidgetClass,colRC,XmNlabelString, xxms,NULL);
   Global.colText = XtVaCreateManagedWidget("00000",
       xmTextFieldWidgetClass,colRC,
       XmNeditable,False,XmNcolumns,6,NULL);

   rowRC = XtVaCreateManagedWidget("row_area",
      xmRowColumnWidgetClass,rowcolRC,
      XmNorientation,XmHORIZONTAL,NULL);
   rowTextLabel = XtVaCreateManagedWidget("row",
       xmLabelWidgetClass,rowRC,XmNlabelString,yxms,NULL);
   Global.rowText = XtVaCreateManagedWidget("00000",
       xmTextFieldWidgetClass,rowRC,
       XmNeditable,False,XmNcolumns,6,NULL);

/*** EASTING AND NORTHING TEXT FIELDS ***/

   coordFrame = XtVaCreateManagedWidget("coord_frame",
      xmFrameWidgetClass,statusRC,NULL);

   coordRC = XtVaCreateManagedWidget("coord_area",
      xmRowColumnWidgetClass,coordFrame,
      XmNorientation,XmVERTICAL,NULL);
   xxms = XmStringCreateSimple(" Easting:");
   yxms = XmStringCreateSimple("Northing:");

   eastRC = XtVaCreateManagedWidget("east_area",
      xmRowColumnWidgetClass,coordRC,
      XmNorientation,XmHORIZONTAL,NULL);
   eastTextLabel = XtVaCreateManagedWidget("east",
       xmLabelWidgetClass,eastRC,XmNlabelString, xxms,NULL);
   Global.eastText = XtVaCreateManagedWidget("00000",
       xmTextFieldWidgetClass,eastRC,
       XmNeditable,False,XmNcolumns,12,NULL);

   northRC = XtVaCreateManagedWidget("north_area",
      xmRowColumnWidgetClass, coordRC,
      XmNorientation,XmHORIZONTAL,NULL);
   northTextLabel = XtVaCreateManagedWidget("north",
       xmLabelWidgetClass,northRC,XmNlabelString,yxms,NULL);
   Global.northText = XtVaCreateManagedWidget("00000",
       xmTextFieldWidgetClass,northRC,
       XmNeditable,False,XmNcolumns,12,NULL);

/*** CATEGORY AND ATTRIBUTE TEXT FIELDS ***/

   catattFrame = XtVaCreateManagedWidget("catatt_frame",
      xmFrameWidgetClass,statusRC,NULL);

   catattRC = XtVaCreateManagedWidget("catatt_area",
      xmRowColumnWidgetClass,catattFrame,
      XmNorientation,XmVERTICAL,NULL);
   xxms = XmStringCreateSimple(" Category:");
   yxms = XmStringCreateSimple("Attribute:");

   catRC = XtVaCreateManagedWidget("cat_area",
      xmRowColumnWidgetClass,catattRC,
      XmNorientation,XmHORIZONTAL,NULL);
   catTextLabel = XtVaCreateManagedWidget("cat",
       xmLabelWidgetClass,catRC,XmNlabelString, xxms,NULL);
   Global.catText = XtVaCreateManagedWidget("00000",
       xmTextFieldWidgetClass,catRC,
       XmNeditable,False,XmNcolumns,15,NULL);

   attRC = XtVaCreateManagedWidget("att_area",
      xmRowColumnWidgetClass,catattRC,
      XmNorientation,XmHORIZONTAL,NULL);
   attTextLabel = XtVaCreateManagedWidget("att",
       xmLabelWidgetClass,attRC,XmNlabelString,yxms,NULL);
   Global.attText = XtVaCreateManagedWidget("00000",
       xmTextFieldWidgetClass,attRC,
       XmNeditable,False,XmNcolumns,15,NULL);

   XmStringFree(xms);
   XmStringFree(xxms);
   XmStringFree(yxms);

return(mainRC);
}
