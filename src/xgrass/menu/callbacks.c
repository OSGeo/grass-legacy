static char rcsid[] = "@(#)XGRASS $Id: callbacks.c,v 0.0.0.1 1992/05/05 14:58:21 kurt Exp kurt $";
/*
 * File: callbacks.c
 *
 * Desc: callback functions relating to f. functions in the config file
 *
 * Auth: Kurt Buehler
 *
 * Date: Tue Oct 22 15:53:47 CDT 1991
 *
 * Modification History:
 *
 */

#include "xgrass.h"

void 
_XgrassExit(w,cld,cad)
    Widget w;
    XtPointer cld;
    XtPointer cad;
{
   XgrassExit(0);
}

void 
_XgExecHist(w,cld,cad)
    Widget w;
    XtPointer cld;
    XtPointer cad;
{
   XgMenuItemRec *mi = (XgMenuItemRec *)cld;
   char **argv;
   int argc;
   int err;

   __XgHistoryAddItem(mi,XG_HISTORY_EXEC_CAPTURE);
   err = 0;
   XgSystem(_XG_Global.applShell,mi->arglist,True,&err,1);
}

void 
_XgExecCaptureHist(w,cld,cad)
    Widget w;
    XtPointer cld;
    XtPointer cad;
{
   XgMenuItemRec *mi = (XgMenuItemRec *)cld;
   char **argv;
   int argc;
   int err;

   __XgHistoryAddItem(mi,XG_HISTORY_EXEC_CAPTURE);
   err = 0;
   XgSystem(_XG_Global.applShell,mi->arglist,False,&err,1);
}

void 
_XgExecCapture(w,cld,cad)
    Widget w;
    XtPointer cld;
    XtPointer cad;
{
   XgMenuItemRec *mi = (XgMenuItemRec *)cld;
   char **argv;
   int argc;
   int err;

   err = 0;
   XgSystem(_XG_Global.applShell,mi->arglist,False,&err,1);
}

void 
_XgExec(w,cld,cad)
    Widget w;
    XtPointer cld;
    XtPointer cad;
{
   XgMenuItemRec *mi = (XgMenuItemRec *)cld;
   char **argv;
   int argc;
   int err;
   int pid;
   Window myWin;

   err = 0;
   pid = XgSystem(_XG_Global.applShell,mi->arglist,True,&err,1);
   if (pid) {
      Atom property;
      Atom type;
      char buf[64];
 
      sprintf(buf,"XG_PARENT.%d\n",pid);
      property = XInternAtom(_XG_Global.display,buf,False);
      type = XInternAtom(_XG_Global.display,"window",False);
      myWin = XtWindow(_XG_Global.applShell);
      XChangeProperty(_XG_Global.display,RootWindow(_XG_Global.display,0),property,type,
		      32, PropModeReplace, &(myWin), 1);
   }
}

void 
_XgXclip(w,cld,cad)
    Widget w;
    XtPointer cld;
    XtPointer cad;
{
   XgMenuItemRec *mi = (XgMenuItemRec *)cld;
   char buf[256];
   int err;
   int argc;
   char **argv;

#ifdef Undefined
   __XgHistoryAddItem(mi,XG_HISTORY_XCLIP);
   sprintf(buf,"xclip %s &",mi->arglist);
   err = 0;
   XgSystem(_XG_Global.applShell, buf, True,&err,1);
#else
   MakeArgValues(mi->arglist,&argc,&argv);
   if (!XClip(argc,argv,_XG_Global.appContext,_XG_Global.display,_XG_Global.applShell,False)) {
       sprintf(buf,"An error occurred with XClip.  Check the XClip interface file.");
       XgWarningDialog(_XG_Global.applShell,buf);
   }
#endif
}

void
_XgDbSet(w,cld,cad)
    Widget w;
    XtPointer cld;
    XtPointer cad;
{
    XgDbSet(NULL,0,False);
}

void
_XgHistoryToggle(w,cld,cad)
    Widget w;
    XtPointer cld;
    XtPointer cad;
{
   XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)cad;
   XmString xmstring;

   if ( cb->set ) {
       xmstring = XmStringCreateSimple("Disable");
       _XG_Global.history_enabled = XG_HISTORY_ENABLE;
   } else {
       _XG_Global.history_enabled = XG_HISTORY_DISABLE;
       __XgFreeHistoryList();
       if (  _XG_Global.historyWidget != (Widget)0 ) {
	   XtDestroyWidget(_XG_Global.historyWidget);
	   _XG_Global.historyWidget == (Widget)0;
       }
       xmstring = XmStringCreateSimple("Enable");
   }
   XtVaSetValues(w,XmNlabelString,xmstring,NULL);
}

void
_XgHistoryClear(w,cld,cad)
    Widget w;
    XtPointer cld;
    XtPointer cad;
{
   if ( _XG_Global.history_enabled == XG_HISTORY_ENABLE ) {
       __XgFreeHistoryList();
       __XgClearHistoryEditor();
   }
}

void
_XgHistoryEdit(w,cld,cad)
    Widget w;
    XtPointer cld;
    XtPointer cad;
{
   if ( _XG_Global.history_enabled == XG_HISTORY_ENABLE &&
	_XG_Global.historyWidget ) 
       __XgHistoryEdit();
}

void
_XgHistoryReplay(w,cld,cad)
{
   if ( _XG_Global.history_enabled == XG_HISTORY_ENABLE )
       __XgHistoryReplay();
}
