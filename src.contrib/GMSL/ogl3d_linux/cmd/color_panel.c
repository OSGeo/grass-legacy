
/* color_panel:
** creation routine used for panel that changes colors
*/

#include  "interface.h"

/*************************************
* the color panel creation routine  **
*************************************/

void 
color_panel (Widget w, data_cell *dc, caddr_t client_data)
{
    int n;

    Arg wargs[30];
    static int first = 1;

    if(first) {

	inform(dc,"Calculating Positions");
	dc->panels[COLOR].im_already_open = 1;
	dc->panels[COLOR].name = XmStringCreateSimple("Color Panel");
	check_space(dc, COLOR, 0);

	n = 0;
	SetPositionArgs(wargs, &n, dc->here.toph, dc->here.botth,
						-1, -1, XmATTACH_FORM);
	dc->panels[COLOR].form = XtCreateManagedWidget("cform",
		xmFormWidgetClass, dc->form_for_aux_control_panel, wargs,n);

	make_title(COLOR,dc, 0);
	closeb(COLOR,dc,0);		

/*--- set the current color and make the colorbutton for the sites---*/	

	n = 0;
	SetPositionArgs(wargs, &n, 40, 60, 30, 70, NULL);
	XtSetArg(wargs[n], XmNbackground,dc->cells[SITES_CELL]); n++;
	dc->toggle_id[S_COLOR] = XtCreateManagedWidget("Sites Color", 
		xmPushButtonWidgetClass, 
		dc->panels[COLOR].form, wargs, n);	

	XtAddCallback(dc->toggle_id[S_COLOR], XmNarmCallback, pop_color,dc);	
	    
	n = 0;
	SetPositionArgs(wargs, &n, 25, -1, 30, -1, XmATTACH_NONE);
	XtCreateManagedWidget("Current Sites Color",xmLabelGadgetClass,
		 dc->panels[COLOR].form, wargs, n);	


/*--- set the current color and make the colorbar for the background ---*/	

	n = 0;
	SetPositionArgs(wargs, &n, 65, 85, 30, 70, NULL);
	dc->toggle_id[BG_COLOR] = XtCreateManagedWidget("Background", 
		xmPushButtonWidgetClass, 
		dc->panels[COLOR].form, wargs, n);	

	XtAddCallback(dc->toggle_id[BG_COLOR], XmNactivateCallback, 
			pop_color,dc);	
	   
/*
	n = 0;
	SetPositionArgs(wargs, &n, 75, -1, 30, -1, XmATTACH_NONE);
	XtCreateManagedWidget("Current Background Color",xmLabelGadgetClass,
		 dc->panels[COLOR].form, wargs, n);	
*/
	
	set_button_colors(dc->toggle_id[BG_COLOR], dc, BG_CELL);

	inform(dc,"Done");
	first = 0;

    }
/*---------------------------------------------------------------------
this section checks to see if the panel is already open and acts    --
accordingly                                                        --
---------------------------------------------------------------------*/
    else if(dc->panels[COLOR].im_already_open){
	dc->constant = COLOR;
	pops(w, dc, COLOR);
	return;
    }

    else {
	inform(dc,"Calculating Positions");
        reshow(dc,COLOR, 0); 
	set_button_colors(dc->toggle_id[BG_COLOR], dc, BG_CELL);
        inform(dc,"Done");
    }

}



