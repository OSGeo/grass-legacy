

/* script_panel:
** the script panel creation routine  
**/

#include "interface.h"


void script_panel(w, dc, call_data)
Widget w;
data_cell *dc;
caddr_t call_data;
{
    int n;
    Arg wargs[30];
    static int first = 1;

    if(first){
	
	inform(dc,"Calculating Positions");

	dc->panels[SCRIPT].im_already_open = 1;
	dc->panels[SCRIPT].name = XmStringCreateSimple("Script Panel");

	check_space(dc,SCRIPT ,0);

	n = 0;
	XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNtopPosition,dc->here.toph); n++;
	XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNbottomPosition,dc->here.botth); n++;
	XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_FORM); n++;
	dc->panels[SCRIPT].form = XtCreateManagedWidget("sform", 
		xmFormWidgetClass, dc->form_for_aux_control_panel, wargs,n);

	make_title(SCRIPT,dc, 0);
	closeb(SCRIPT,dc,0);

	n = 0;
	SetPositionArgs(wargs, &n, 10, 27, 3, 67, NULL);
	dc->Sscript_file = XtCreateManagedWidget("Script File", 
		xmTextWidgetClass, dc->panels[SCRIPT].form, wargs, n);

	n = 0;
	SetPositionArgs(wargs, &n, 30, 47, 3, 67, NULL);
	dc->Sbegin_read = XtCreateManagedWidget("Begin Read", 
		xmPushButtonWidgetClass, dc->panels[SCRIPT].form, wargs, n);

	n = 0;
	SetPositionArgs(wargs, &n, 50, 67, 3, 67, NULL);
	dc->Sstop_read = XtCreateManagedWidget("Stop Read", 
		xmPushButtonWidgetClass, dc->panels[SCRIPT].form, wargs, n);

	inform(dc,"Done");

	first = 0;
    }


    else if(dc->panels[SCRIPT].im_already_open){
	dc->constant = SCRIPT;
	pops(w, dc, SCRIPT);
	return;
    }


    else {
	inform(dc,"Calculating Positions");
	reshow(dc,SCRIPT, 0);
	inform(dc,"Done");
    }

}


