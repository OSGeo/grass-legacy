#include "digit.h"



make_edit_menu (parent)
    Widget parent;
{
    Widget rmvline, rmvsite, rmvpoint, addpoint, snapl, breakl;
    Widget movpt, movlos, retypel, rmvblock, pull_panel, pull;
    int    x, y;
    int    i, n;
    Arg    wargs[10];
    Pixel  fg, bg;
    int xinc = 9;

    if (Text)
	xinc = 20;



    x = 10;
    y = 5;
   
    n = 0;
    XtSetArg (wargs[n], XtNforeground, &fg); n++;
    XtSetArg (wargs[n], XtNbackground, &bg); n++;
    XtGetValues (parent, wargs, n);
 
    rmvline = 
    make_button (parent, "remove line", NULL, NULL, "rmline", fg, bg, "Remove line");
    XtAddCallback (rmvline, XmNactivateCallback, check_changes, NULL);

    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (rmvline, wargs, n); 
    
    XtAddCallback (rmvline,XmNactivateCallback, Edit, MEC_REMOVE);
    rmvsite = 
    make_button (parent, "remove site", NULL, NULL, "rmsite", fg, bg, "Remove site");
    XtAddCallback (rmvsite, XmNactivateCallback, check_changes, NULL);

    x += xinc;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (rmvsite, wargs, n); 
    XtAddCallback (rmvsite,XmNactivateCallback, Edit, MEC_RMVSIT);
    
    rmvblock = 
    make_button (parent, "remove block", NULL, NULL,"rmblock", fg, bg,
					    "Remove block of lines");
    XtAddCallback (rmvblock, XmNactivateCallback, check_changes, NULL);
    x += xinc;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (rmvblock, wargs, n); 
    XtAddCallback (rmvblock,XmNactivateCallback, Edit, MEC_BLOCK);
    
#ifdef RMPT
    rmvpoint = 
    make_button (parent, "remove point", NULL, NULL, "rmpoint", fg, bg,
						 "Remove point");
    XtAddCallback (rmvpoint, XmNactivateCallback, check_changes, NULL);
    x += xinc;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (rmvpoint, wargs, n); 
    XtAddCallback (rmvpoint, XmNactivateCallback, Edit, MEC_REMOVE);
    
#endif
    breakl = 
    make_button (parent, "break", NULL, NULL, "break", fg, bg, "Break a line");
    XtAddCallback (breakl, XmNactivateCallback, check_changes, NULL);
    x += xinc;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (breakl, wargs, n); 
    XtAddCallback (breakl, XmNactivateCallback, Edit, MEC_BREAK);
    
    retypel =
    make_button (parent, "retype", NULL, NULL, "retype", fg, bg, "Retype a line");
    XtAddCallback (retypel, XmNactivateCallback, check_changes, NULL);
    if(Text)
    {
       x = 10;
       y = 50;
    }
    x += xinc;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (retypel, wargs, n); 
    
    XtAddCallback (retypel, XmNactivateCallback, Edit, MEC_TYPE);
    
    snapl =  
    make_button (parent, "snap", NULL, NULL, "snap", fg, bg,
						  "Snap line to node");
    x += xinc;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (snapl, wargs, n); 
    
    XtAddCallback (snapl, XmNactivateCallback, Edit, MEC_SNAP);
#ifdef ADDPT
    addpoint =
    make_button (parent, "add point", NULL, NULL, "addpt", fg, bg, "Add a point");
    XtAddCallback (addpoint, XmNactivateCallback, check_changes, NULL);
    x += xinc;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (addpoint, wargs, n); 
#endif 
    movpt =
    make_button (parent, "move point", NULL, NULL, "mvpt", fg, bg,
							"Move a point");
    XtAddCallback (movpt, XmNactivateCallback, check_changes, NULL);
    x += xinc;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (movpt, wargs, n); 
    XtAddCallback (movpt, XmNactivateCallback, Edit, MEC_MOVE);
    
    movlos =
    make_button (parent, "move line", NULL, NULL, "mvlos", fg, bg,
					"Move a line or site");
    XtAddCallback (movlos, XmNactivateCallback, check_changes, NULL);
    x += xinc;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (movlos, wargs, n); 
    XtAddCallback (movlos, XmNactivateCallback, Edit, MEC_MOVEL);
   
#ifdef  CURVE_EDIT
    pull_panel = install_control_panel();
    pull =
    make_button (parent, "edit curve", show_pull, pull_panel, NULL, fg, bg,
					"Edit line");
    
    x += xinc;
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (pull, wargs, n); 

    Pull_flag = 1;
#else
    Pull_flag = 0;
#endif /* CURVE_EDIT */
}

Widget
make_dialog (parent, name, str, manage) 
    Widget parent;
    char   *name, *str;
    int    manage;
{
    int n, x, y;
    Arg wargs[10];
    XmString  message, accept, title;
    Widget w;

    message = XmStringCreateSimple (str);
    title = XmStringCreateSimple (name);
    accept = XmStringCreateSimple ("accept");

    n = 0;
    w = XmCreateMessageDialog (toplevel, name, NULL, 0);
    XtVaSetValues(w, XmNautoUnmanage, manage, 
    		    XmNmessageString, message,
    		XmNokLabelString, accept,
    	    XmNdialogTitle, title, 
	     NULL); 
    XtVaSetValues (XtParent(w), 
		XmNsaveUnder, True,
		NULL);
   
    if (!manage)
    XtAddCallback (w, XmNcancelCallback, downcb, w);
    
    XmStringFree (message);
    XmStringFree (title);
    XmStringFree (accept);
    
    XtUnmanageChild (XmMessageBoxGetChild(w, XmDIALOG_HELP_BUTTON));
    return w;

}

