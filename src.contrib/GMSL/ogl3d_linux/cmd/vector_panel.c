#include  "interface.h"

/* vector_panel:
** the vector panel creation routine 
*/

void 
vector_panel (Widget w, data_cell *dc, caddr_t client_data)
{
    int i,n;
    Arg wargs[30];
    static int first = 1;
    Widget wtmp, title, rad, wwrc;
    char **names;

    if(first) {
	inform(dc,"Calculating Positions");
	dc->panels[VECTOR].im_already_open = 1;
	dc->panels[VECTOR].name = XmStringCreateSimple("Vector Panel");

	check_space(dc, VECTOR,0);

	n = 0;
	SetPositionArgs(wargs, &n, dc->here.toph, dc->here.botth,
						-1, -1, XmATTACH_FORM);
	dc->panels[VECTOR].form = XtCreateManagedWidget("vform", 
		xmFormWidgetClass, dc->form_for_aux_control_panel, wargs,n);

	title = make_title(VECTOR, dc, 0);


/* make Option menu for selecting current vector */

	n = 0;
	SetPositionArgs(wargs, &n, 15,-1,2,-1, XmATTACH_NONE);
	dc->RCurVectno = make_simple_options(dc->panels[VECTOR].form, 
		    "wcur_v", "Current:", v_labels(dc), 
		    GV_num_vects(), dc->CurVect, set_cur_vect, wargs, n);

/* make new & delete buttons */

	n = 0;
	SetPositionArgs(wargs, &n, 15,30,70,82, NULL);
	wtmp = XtCreateManagedWidget("New", xmPushButtonWidgetClass, dc->panels[VECTOR].form, wargs, n);
	XtAddCallback(wtmp, XmNactivateCallback,new_vect,dc);	

	n = 0;
	SetPositionArgs(wargs, &n, 15,30,83,99, NULL);
	wtmp = XtCreateManagedWidget("Delete", 
		  xmPushButtonWidgetClass, dc->panels[VECTOR].form, wargs, n);
	XtAddCallback(wtmp, XmNactivateCallback,delete_vect,dc);	

/* make Arrows and text for selecting line width */

	n = 0;
	SetPositionArgs(wargs, &n, 35, -1, 4, -1, XmATTACH_NONE);
	make_arrows  (dc->panels[VECTOR].form, "Line Width",
		    1, 15, 2, &(dc->parrows[VWIDTH_ARWS]), wargs, &n);
	XtAddCallback(dc->parrows[VWIDTH_ARWS].up, 
		    XmNactivateCallback,vect_set_displaymode,dc);	
	XtAddCallback(dc->parrows[VWIDTH_ARWS].down, 
		    XmNactivateCallback,vect_set_displaymode,dc);	

	n = 0;
	SetPositionArgs(wargs, &n, 65, -1, 4, -1, XmATTACH_NONE);
	XtSetArg(wargs[n],XmNset,TRUE); n++;
	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	dc->toggle_id[VECT_MEM] = XtCreateManagedWidget("Load to memory", 
		    xmToggleButtonGadgetClass,
		    dc->panels[VECTOR].form, wargs, n);
	XtAddCallback(dc->toggle_id[VECT_MEM], XmNdisarmCallback,
			vect_set_displaymode, dc);
	
/* make PushButton for selecting line color */

	n = 0;
	SetPositionArgs(wargs, &n, 85, -1, 45, 65, NULL);
/*
	SetPositionArgs(wargs, &n, 77, 90, 5, 25, NULL);
	XtSetArg(wargs[n], XmNbackground,dc->cells[VECT_CELL]); n++;
*/
	dc->toggle_id[V_COLOR] = XtCreateManagedWidget("Color", 
		xmPushButtonWidgetClass, 
		dc->panels[VECTOR].form, wargs, n);	

	XtAddCallback(dc->toggle_id[V_COLOR],XmNactivateCallback,pop_color,dc);	

	set_button_colors(dc->toggle_id[V_COLOR], dc, VECT_CELL);
	  

/* 
	n = 0;
	SetPositionArgs(wargs, &n, 86, -1, 10, -1, XmATTACH_NONE);
	wtmp = XtCreateManagedWidget("Qdraw", xmPushButtonWidgetClass, 
		dc->panels[VECTOR].form, wargs, n);	

	XtAddCallback(wtmp,XmNactivateCallback,do_fastvectdraw,dc);

Button to test vect decimation - will become positioning panel for vects */


/* make ScrolledWindow with RowCol containing check boxes to mark
   which surfaces current vector is to be displayed on */

	n = 0;
	SetPositionArgs(wargs, &n, 33, 41, 45, -1, XmATTACH_NONE);
	XtCreateManagedWidget("Display on surface(s):",xmLabelGadgetClass,
		 dc->panels[VECTOR].form, wargs, n);	

	n = 0;
	SetPositionArgs(wargs, &n, 45, 85, 50, 95, XmATTACH_NONE);
	XtSetArg(wargs[n], XmNscrollingPolicy,XmAUTOMATIC); n++;
	dc->Vscroll = XmCreateScrolledWindow(dc->panels[VECTOR].form, 
		 "on_surfs", wargs, n);	

	n = 0;
	wwrc = XtCreateManagedWidget("surf_buttons", xmRowColumnWidgetClass,
		    dc->Vscroll, wargs, n);
	
	names = sf_labels(dc);
	for(i=0; i < GS_num_surfs(); i++){
	    n = 0;
	    if(GV_surf_is_selected(dc->hVect[dc->CurVect], dc->hSurf[i])){
		XtSetArg(wargs[n],XmNset,TRUE); n++;
	    }
	    XtCreateManagedWidget(names[i], xmToggleButtonGadgetClass,
		    wwrc, wargs, n);
	}

	n = 0;
	XtSetArg(wargs[n], XmNworkWindow,wwrc); n++;
	XtSetValues(dc->Vscroll, wargs, n);

	XtManageChild(dc->Vscroll);
	    
	closeb(VECTOR,dc,0);
	
	vect_update_displaymode(NULL, dc, NULL);

	inform(dc,"Done");

	first = 0;
    }


/*---------------------------------------------------------------------
this section checks to see if the panel is already open and acts    --
accordingly                                                        --
---------------------------------------------------------------------*/

    else if(dc->panels[VECTOR].im_already_open){
	dc->constant = VECTOR;
	pops(w, dc, VECTOR);
	return;
    }

    else {
        inform(dc,"Calculating Positions");	
	_update_vector_options(dc);
	_update_Vscroll_options(dc);  /* combine these two? */
        reshow(dc,VECTOR, 0); 
        inform(dc,"Done");	
    }

}


