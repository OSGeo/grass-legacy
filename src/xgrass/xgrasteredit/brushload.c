
/*
 * FILE: brushload.c 
 *
 * PROGRAMMER: David M. Johnson
 * 
 * FUNCTIONS:
 *
 * BrushLoad()
 * -----------
 * This function sets up the brush-load dialog using an XGRASS 
 * browser widget so that the user may choose a saved brush to
 * be loaded.
 *
 * brushLoadOkCB()
 * ---------------
 * This is the callback for the brush-load dialog, it calls 
 * loadbrush() to... 
 *
 * loadbrush()
 * -----------
 * This function loads the brush indicated by the brush name 
 * (bname) and mapset (bmapset) parameters into the global 
 * brush structure (Global.brush).
 * 
 */

#include "Browser.h"
#ifdef SVR4
#include "string.h"
#else
#include "strings.h"
#endif
#include "xgre.h"

/*** LOCAL GLOBALS ***/

Widget loadForm;               /* toplevel form */

/*** LOCAL PROTOTYPES ***/

int loadbrush(
#ifndef _NO_PROTO
   char *bname,
   char *bmapset
#endif
);

void brushLoadOkCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

void brushLoadCancelCB(
#ifndef _NO_PROTO
   Widget w,
   XtPointer cld,
   XtPointer cad
#endif
);

/*****************/
/*** BrushLoad ***/
/*****************/

void
#ifdef _NO_PROTO
BrushLoad(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
BrushLoad(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Arg al[15];
int ac = 0;
XmString xms,xmapset;

if (Global.FbrushLoadD) return;
Global.FbrushLoadD = True;

/*** BRUSH BROWSER ***/

xms = XmStringCreateSimple("Select Brush to Load");
xmapset = XmStringCreateSimple(G_mapset());
XtSetArg(al[ac],XmNpromptLabelString,xms); ac++;
XtSetArg(al[ac],XmNokLabelString,XmStringCreateSimple("Load")); ac++;
XtSetArg(al[ac],XmNinitialMapset1,xmapset); ac++;
XtSetArg(al[ac],XmNnumLists, 1); ac++;
XtSetArg(al[ac],XmNbrowseMode,XG_USER_DEFINED); ac++;
XtSetArg(al[ac],XmNuserDBElement,XmStringCreateSimple("brush")); ac++;
XtSetArg(al[ac],XmNselMode, XG_SINGLE_SELECT); ac++;
Global.brushLoadD=XgCreateBrowserDialog(Global.applShell,"BrushLoad",al,ac);
XtAddCallback(Global.brushLoadD,XmNokCallback,brushLoadOkCB,cld);
XtAddCallback(Global.brushLoadD,XmNcancelCallback,brushLoadCancelCB,NULL);
XtManageChild(XgInteractorGetChild(Global.brushLoadD,XmINTERACT_PROMPT_LABEL));
XmStringFree(xms);

/* FIX: set default buttons
XtVaSetValues(Global.brushLoadD,XmNdefaultButton,
   XgInteractorGetChild(Global.brushLoadD,XmINTERACT_OK_BUTTON),NULL);*/

XtManageChild(Global.brushLoadD);
}

/*********************/
/*** brushLoadOkCB ***/
/*********************/

void
#ifdef _NO_PROTO
brushLoadOkCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
brushLoadOkCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
char *result;
char *p1, *p2;
XmString xms;
char msg[1000];

XtVaGetValues(Global.brushLoadD, XmNresultString, &xms, NULL);
XmStringGetLtoR(xms,XmSTRING_DEFAULT_CHARSET,&result);

if ( *result != NULL )
   {
   /* set globals for raster map name and mapset */
   p1 = (char*)strtok(result,"@"); p2 = (char*)0;
   strcpy(Global.bname,p1);
   p1 = (char*)strtok(p2,"@"); 
   strcpy(Global.bmapset,p1);

   loadbrush(Global.bname,Global.bmapset); 

   KillBrushDialogs();
   Global.FbrushLoadD = False;

   sprintf(msg,"BRUSH: %dx%d (%s)",
      Global.brushRows,Global.brushCols,Global.bname);
   UpdateBrushText(msg);
   }
}

/*****************/
/*** loadbrush ***/
/*****************/

int loadbrush(bname,bmapset)
char *bname;
char *bmapset;
{
FILE *bfp;
int  bx,by;
char path[200];
char buf[1000];
char msg[100];
char *p1, *p2;
int  gotRows=0, 
     gotCols=0, 
     gotHRow=0, 
     gotHCol=0,
     readError=0;

#ifdef DEBUG
printf("loadbrush: %s@%s\n",bname,bmapset);
#endif

if ((bfp = G_fopen_old("brush",bname,bmapset)) == NULL)
   {
   sprintf(msg,"Unable to open brush file <%s> for reading",bname);
   XgError(Global.brushLoadD,msg);
   return(-1);
   }

while (1)
   {
   if (fgets(buf,1000,bfp) == NULL) break;
   if (strncmp(buf,"ROWS:",5)==0)
      {
      p1 = (char*)strtok(buf,":"); p2 = (char*)0;
      p1 = (char*)strtok(p2,":");
      if (sscanf(p1,"%d",&(Global.brushRows)) == 1) gotRows = 1;
      }
   else if (strncmp(buf,"COLS:",5)==0)
      {
      p1 = (char*)strtok(buf,":"); p2 = (char*)0;
      p1 = (char*)strtok(p2,":");
      if (sscanf(p1,"%d",&(Global.brushCols)) == 1) gotCols = 1;
      }
   else if (strncmp(buf,"HROW:",5)==0)
      {
      p1 = (char*)strtok(buf,":"); p2 = (char*)0;
      p1 = (char*)strtok(p2,":");
      if (sscanf(p1,"%d",&(Global.brushHotRow)) == 1) gotHRow = 1;
      }
   else if (strncmp(buf,"HCOL:",5)==0)
      {
      p1 = (char*)strtok(buf,":"); p2 = (char*)0;
      p1 = (char*)strtok(p2,":");
      if (sscanf(p1,"%d",&(Global.brushHotCol)) == 1) gotHCol = 1;
      }
   else if (strncmp(buf,"BRUSH:",6)==0)
      {
      if (gotRows && gotCols && gotHRow && gotHCol)
         {
         for (by=0; by<Global.brushRows; by++)
            {
            if (fgets(buf,1000,bfp) == NULL) 
               {
               readError = 1;
               break; 
               }
            p1 = (char*)strtok(buf," "); p2 = (char*)0;

            for (bx=0; bx<Global.brushCols; bx++)
               {
               int tinput, top, tvalue;
               if (sscanf(p1,"%d:%d:%d",&tinput,&top,&tvalue) == 3)
                  {
                  Global.brush[bx][by].input = tinput;
                  Global.brush[bx][by].op    = top;
                  Global.brush[bx][by].value = tvalue;
                  }
               else readError = 2;
               p1 = (char*)strtok(p2," ");
               } 
            } 
         }
      else readError = 3;
      }
   }

if (readError)
   {
   sprintf(msg,"Error reading brush file <%s> %d",bname,readError);
   XgError(Global.brushLoadD,msg);
   return(-1);
   }

fclose(bfp);
return(-1);
}

/*************************/
/*** brushLoadCancelCB ***/
/*************************/

void
#ifdef _NO_PROTO
brushLoadCancelCB(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
brushLoadCancelCB(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Global.FbrushLoadD = False;
XtDestroyWidget(w);
}

