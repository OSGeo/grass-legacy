

/* script_panel:
** the script panel creation routine  
**/

#include "interface.h"


void animation_panel(w, dc, call_data)
Widget w;
data_cell *dc;
caddr_t call_data;
{
    int n;
    Arg wargs[30];
    static int first = 1;

    if(first){
	
	fprintf(stderr,"This is test 1\n");
	
	/* inform(dc,"Calculating Positions"); */

	dc->panels[ANIM].im_already_open = 1;
	
	dc->panels[ANIM].name = XmStringCreateSimple("Animation Panel");
        
	check_space(dc,ANIM ,0);

	n = 0;
	XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNtopPosition,dc->here.toph); n++;
	XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNbottomPosition,dc->here.botth); n++;
	XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_FORM); n++;
	dc->panels[ANIM].form = XtCreateManagedWidget("sform", 
		xmFormWidgetClass, dc->form_for_aux_control_panel, wargs,n);

	make_title(ANIM,dc, 0);
	closeb(ANIM,dc,0);

	n = 0;
	SetPositionArgs(wargs, &n, 10, 27, 3, 67, NULL);
	dc->Sscript_file = XtCreateManagedWidget("Anim File", 
		xmTextWidgetClass, dc->panels[ANIM].form, wargs, n);

	n = 0;
	SetPositionArgs(wargs, &n, 30, 47, 3, 67, NULL);
	dc->Sbegin_read = XtCreateManagedWidget("Bgin Read", 
		xmPushButtonWidgetClass, dc->panels[ANIM].form, wargs, n);

	n = 0;
	SetPositionArgs(wargs, &n, 50, 67, 3, 67, NULL);
	dc->Sstop_read = XtCreateManagedWidget("Stp Read", 
		xmPushButtonWidgetClass, dc->panels[ANIM].form, wargs, n);

	inform(dc,"Done");

	first = 0;
    }


    else if(dc->panels[ANIM].im_already_open){
	dc->constant = ANIM;
	pops(w, dc, ANIM);
	return;
    }


    else {
	inform(dc,"Calculating Positions");
	reshow(dc,ANIM, 0);
	inform(dc,"Done");
    }

}


