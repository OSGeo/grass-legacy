/* gets existing or new file name, base directory = current working */

#include "interface.h"
#include <Xm/FileSB.h>



Widget 
make_fsbox (Widget parent, char *title, char *prompt, char *pattern, char *dir, XtCallbackProc cb, data_cell *dc)
{
  Widget w, _selection_label;
  Arg wargs[8];
  int n;

  n=0; 
  if(pattern){
      XtSetArg(wargs[n],XmNpattern,
	  (XtArgVal)XmStringCreateLtoR (pattern,XmSTRING_DEFAULT_CHARSET)); 
      n++;
  }
  if(dir){
      XtSetArg(wargs[n],XmNdirectory,
	  (XtArgVal)XmStringCreateLtoR (dir,XmSTRING_DEFAULT_CHARSET)); 
      n++;
  }
  XtSetArg(wargs[n],XmNdialogTitle,
	  (XtArgVal)XmStringCreateLtoR (title,XmSTRING_DEFAULT_CHARSET)); n++;
  XtSetArg(wargs[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;

  /* Create the FileSelectionBox with OK and Cancel buttons. */
  w = XmCreateFileSelectionDialog (parent, "fsb", wargs, n);
  XtUnmanageChild (XmFileSelectionBoxGetChild (w, XmDIALOG_HELP_BUTTON));
  XtAddCallback (w, XmNokCallback, cb, dc);
  XtAddCallback (w, XmNcancelCallback, destroy_cb, w);

  /* Set selection label to specified prompt. */
  n=0;
  _selection_label = 
	   XmFileSelectionBoxGetChild (w, XmDIALOG_SELECTION_LABEL);
  XtSetArg(wargs[n],XmNlabelString, (XtArgVal)XmStringCreateLtoR (prompt,
	   XmSTRING_DEFAULT_CHARSET)); n++;
  XtSetValues (_selection_label, wargs, n);

  return w;
}
