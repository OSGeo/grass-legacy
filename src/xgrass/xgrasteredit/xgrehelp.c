
#include "Help.h" 
#include "xgre.h"

/*****************/
/*** HelpModal ***/
/*****************/

void
#ifdef _NO_PROTO
HelpModal(w, cld, cad)
   Widget w;
   XtPointer cld, cad;
#else
HelpModal(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Arg al[20];
int ac;
Widget helpModal;
char *filename[200];

switch (Global.mode)
   {
   case XGRE_UNLOADED:
      sprintf(filename,"xgre/mode_unloaded");
      break;
   case XGRE_LOCKED:
      sprintf(filename,"xgre/mode_locked");
      break;
   case XGRE_NORMAL:
      sprintf(filename,"xgre/mode_normal");
      break;
   case XGRE_VERTI_DRAG:
      sprintf(filename,"xgre/mode_verti");
      break;
   case XGRE_HORIZ_DRAG:
      sprintf(filename,"xgre/mode_horiz");
      break;
   case XGRE_BOX_EDIT:
      sprintf(filename,"xgre/mode_box");
      break;
   case XGRE_POLY_EDIT:
      sprintf(filename,"xgre/mode_poly");
      break;
   }

ac=0;
XtSetArg(al[ac],XmNhelpFile,filename); ac++;
XtSetArg(al[ac],XmNdismissOnly,True); ac++;
XtSetArg(al[ac],XmNhotwordForeground,XgdGetVectColorPixelByName("green")); ac++;
helpModal = XgCreateHelpDialog(Global.applShell,"helpModal",al,ac);
XtManageChild(helpModal);
}

/*******************/
/*** HelpGeneral ***/
/*******************/

void
#ifdef _NO_PROTO
HelpGeneral(w, cld, cad)
   Widget w;
   XtPointer cld, cad;
#else
HelpGeneral(Widget w, XtPointer cld, XtPointer cad)
#endif
{
Arg al[20];
int ac;
Widget helpGeneral;

ac=0;
XtSetArg(al[ac],XmNhelpFile,"xgre/intro"); ac++;
XtSetArg(al[ac],XmNdismissOnly,True); ac++;
XtSetArg(al[ac],XmNhotwordForeground,XgdGetVectColorPixelByName("green")); ac++;
helpGeneral = XgCreateHelpDialog(Global.applShell,"helpGeneral",al,ac);
XtManageChild(helpGeneral);
}


