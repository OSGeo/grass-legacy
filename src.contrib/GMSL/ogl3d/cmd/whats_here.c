
/* whats_here_panel:
** creates the panel used for the user to identify image coordinates
*/


#include "interface.h"


/******************************************
* the whats_here panel creation routine  **
******************************************/
void 
whats_here_panel (Widget w, data_cell *dc, caddr_t call_data)
{
    int n;
    Arg wargs[30];
    Widget title, sep, clr;
    static int first = 1;

    if(first){

	inform(dc,"Calculating Positions");
	dc->panels[WHAT].im_already_open = 1;
	dc->panels[WHAT].name = XmStringCreateSimple("What's Here Panel");

	check_space(dc,WHAT, 0);

	n = 0;
	XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNtopPosition,dc->here.toph); n++;
	XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNbottomPosition,dc->here.botth); n++;
	XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_FORM); n++;
	dc->panels[WHAT].form = XtCreateManagedWidget("sform", 
		xmFormWidgetClass, dc->form_for_aux_control_panel, wargs,n);

	title = make_title(WHAT,dc, 0);


/*------ make the text widget to display the info ------*/
	n = 0;
	SetPositionArgs(wargs, &n, 45, 90, 2, 96, NULL);
	XtSetArg(wargs[n],XmNeditable,FALSE); n++;
	XtSetArg(wargs[n],XmNwordWrap,TRUE); n++;
	XtSetArg(wargs[n],XmNeditMode,XmMULTI_LINE_EDIT); n++;
	XtSetArg(wargs[n],XmNscrollingPolicy, XmAUTOMATIC); n++;
	dc->Wtxt = XmCreateScrolledText(dc->panels[WHAT].form, "Wtxt", 
								wargs, n);

/*-----------------------------------------------------------------------
make the toggle button to determine if whats here is active or not ---- 
-----------------------------------------------------------------------*/
	n = 0;
	XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(wargs[n],XmNbottomOffset,3); n++;
	XtSetArg(wargs[n],XmNbottomWidget,dc->Wtxt); n++;
	XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNleftPosition,3); n++;
	XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNrightPosition,40); n++;
	XtSetArg(wargs[n],XmNshadowType,XmSHADOW_OUT); n++;
	dc->Wframe = XtCreateManagedWidget("What's Here?", xmFrameWidgetClass, 
		dc->panels[WHAT].form, wargs, n);

	n = 0;
	XtSetArg(wargs[n],XmNindicatorType,XmN_OF_MANY); n++;
	dc->Wwhatshere = XtCreateManagedWidget("What's Here?", 
		xmToggleButtonWidgetClass, dc->Wframe, wargs, n);

	XtAddCallback(dc->Wwhatshere,XmNarmCallback,add_me,dc);


/*-----------------------------------------------
make the push button to separate the data  ---- 
-----------------------------------------------*/
	n = 0;
	XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(wargs[n],XmNbottomOffset,3); n++;
	XtSetArg(wargs[n],XmNbottomWidget,dc->Wtxt); n++;
	XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNleftPosition,45); n++;
	sep = XtCreateManagedWidget("Separate", xmPushButtonWidgetClass, 
		dc->panels[WHAT].form, wargs, n);

	XtAddCallback(sep,XmNactivateCallback,separate,dc);


/*-----------------------------------------------
make the push button to clear the widget   ---- 
-----------------------------------------------*/
	n = 0;
	XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(wargs[n],XmNbottomOffset,3); n++;
	XtSetArg(wargs[n],XmNbottomWidget,dc->Wtxt); n++;
	XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(wargs[n],XmNleftOffset,3); n++;
	XtSetArg(wargs[n],XmNleftWidget,sep); n++;
	clr = XtCreateManagedWidget("Clear", xmPushButtonWidgetClass, 
		dc->panels[WHAT].form, wargs, n);

	XtAddCallback(clr,XmNactivateCallback,cleartxt,dc);


	XtManageChild(dc->Wtxt);

	closeb(WHAT,dc,0);

	inform(dc,"Done");

	first = 0;
    }

    else if(dc->panels[WHAT].im_already_open){
	pops(w, dc, WHAT);
	return;
    }

    else {
	inform(dc,"Calculating Positions");
	reshow(dc,WHAT, 0);
	inform(dc,"Done");		
    }
}


