
#include "xgre.h"

/**************************/
/*** UpdatePositionText ***/
/**************************/

void 
#ifdef _NO_PROTO
UpdatePositionText(x,y)
   int x,y;
#else
UpdatePositionText(int x, int y)
#endif
{
int proj = G_projection();
char xstr[20], ystr[20];

   sprintf(xstr,"%d",x);
   sprintf(ystr,"%d",y);
   XmTextFieldSetString(Global.colText,xstr);
   XmTextFieldSetString(Global.rowText,ystr);
   /* FIX: needs to handle Lat/Lon as well */
   if (Global.mode!=XGRE_UNLOADED && proj!=PROJECTION_LL)
      {
      char buf1[50], buf2[50]; float fx,fy;
      fx = G_col_to_easting((double)x,&(Global.seghd));
      fy = G_row_to_northing((double)y,&(Global.seghd));
      G_format_easting(fx,buf1,proj);
      G_format_northing(fy,buf2,proj);
      XmTextFieldSetString(Global.eastText,buf1);
      XmTextFieldSetString(Global.northText,buf2);
      }
}

/**************************/
/*** UpdateCategoryText ***/
/**************************/

void 
#ifdef _NO_PROTO
UpdateCategoryText(x,y)
   int x,y;
#else
UpdateCategoryText(int x, int y)
#endif
{
CELL value;
char str[2048];
char *label;

if (segment_get(&(Global.seg),(char*)&value,y,x))
   {
   /* set category-number text field */
   sprintf(str,"%d",value);
   XmTextFieldSetString(Global.catText,str);

   /* set attribute-string text field */ 
   label = G_get_cat((CELL)value,&(Global.cats));
   if (*label == NULL)
      sprintf(str,"no-label");
   else
      sprintf(str,"%s",label);
   XmTextFieldSetString(Global.attText,str); 
   }
}

/***********************/
/*** UpdateBrushText ***/
/***********************/

void 
#ifdef _NO_PROTO
UpdateBrushText(msg)
   char *msg;
#else
UpdateBrushText(char *msg)
#endif
{
XmTextFieldSetString(Global.brushText,msg);
}

/**********************/
/*** UpdateModeText ***/
/**********************/

void
#ifdef _NO_PROTO
UpdateModeText(msg)
   char *msg;
#else
UpdateModeText(char *msg)
#endif
{
XmTextFieldSetString(Global.modeText,msg);
}

