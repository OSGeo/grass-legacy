#include "xgdisp.h"

void 
#ifdef _NO_PROTO
canceldialog(w, cld, cad)
Widget w;
XtPointer cld;
XtPointer cad;
#else
canceldialog(Widget w, XtPointer cld, XtPointer cad)
#endif
{
XtDestroyWidget (XtParent(w));
}

void
#ifdef _NO_PROTO
getsitepixfile(w, cld, cbs)
Widget w;
XtPointer cld;
XmFileSelectionBoxCallbackStruct *cbs;
#else
getsitepixfile(Widget w, XtPointer cld, XmFileSelectionBoxCallbackStruct *cbs)
#endif
{
char *filename;

     if (cbs !=NULL && !XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, 
		&filename ))
	return; 

     if (!*filename) {
	XtFree(filename);
	XgWarningDialog(Global.applShell, "No Pixmap file selected");
	return;
	}

     Global.sitefile = (char *)XtMalloc(sizeof (char)*strlen(filename) + 1);
     strcpy (Global.sitefile, filename);
	
}

void
#ifdef _NO_PROTO
XgdSelectPixmap()
#else
XgdSelectPixmap(void)
#endif
{
  Arg al[10];
  int ac = 0;
  Widget fileseldialog;
  XmString xms;
  char filename[1024];
  
  
  sprintf (filename, ".\/*.xpm");
  xms = XmStringCreateSimple(filename);
  
  XtSetArg(al[ac], XmNdirMask, xms); ac++; 
  XtSetArg(al[ac], XmNautoUnmanage, True); ac++;
  fileseldialog = XmCreateFileSelectionDialog(Global.applShell,
					      "Icon File Selection",
					      al, ac);
	   
  XtAddCallback(fileseldialog, XmNcancelCallback, canceldialog, NULL); 
  XtAddCallback(fileseldialog, XmNokCallback, getsitepixfile, NULL); 
  
  XtManageChild(fileseldialog);
}

