
/* light_panel:
** creation routine for the light panel
*/

#include "interface.h"

void 
light_panel (Widget w, data_cell *dc, caddr_t call_data)
{
    int n, test;
    Arg wargs[30];
    
    static int first = 1;


    if(first){
    
	inform(dc,"Calculating Positions");

	dc->panels[LIGHT].im_already_open = 1;
	dc->panels[LIGHT].name = XmStringCreateSimple("Light Panel");

	check_space(dc, LIGHT, 1);

	n = 0;
	SetPositionArgs(wargs, &n, TOP, FORM, -1, -1, XmATTACH_FORM);
	dc->panels[LIGHT].form = XtCreateManagedWidget("lform", 
		xmFormWidgetClass, dc->form_for_aux_control_panel, wargs,n);

	make_title(LIGHT,dc, 1);

	n = 0;
	SetPositionArgs(wargs, &n, 10, 15, 2, -1, XmATTACH_NONE);
	dc->Lfollow_view = XtCreateManagedWidget("Follow ViewPoint", 
		xmToggleButtonWidgetClass, dc->panels[LIGHT].form, wargs, n);
	dc->toggle_id[FOLLOW_VIEW_LGT] = dc->Lfollow_view;

	n = 0;
	SetPositionArgs(wargs, &n, 15, 20, 2, -1, XmATTACH_NONE);
	dc->Lshow_model = XtCreateManagedWidget("Show Model", 
		xmToggleButtonWidgetClass, dc->panels[LIGHT].form, wargs, n);
	dc->toggle_id[SHOW_MODEL_LGT] = dc->Lshow_model;

/*---------------------------------------------------------------------
Here the sliders are placed and created.  when a form widget was    --
used as a parent for a slider, it kept bailing out of the edge      --
sync loop. so I made a rowcol widget to hold the sliders            --
and to make a long story short, I eventually had to make a rowcol   --
to hold each slider.  See the documentation in Quirks for more info   --
----------------------------------------------------------------------*/

	n = 0;
	SetPositionArgs(wargs, &n, 21, -1, 2, 44, XmATTACH_NONE);
	XtSetArg (wargs[n], XmNorientation, XmHORIZONTAL); n++;
	dc->Lbright_rc = XtCreateManagedWidget("bright_rc", 
		xmRowColumnWidgetClass, dc->panels[LIGHT].form, wargs, n);

	    
	n = 0;
	XtSetArg(wargs[n],XmNscaleHeight,20); n++;
	XtSetArg(wargs[n],XmNscaleWidth,115); n++;
	dc->sliders[LITE_BGT] = make_slider(dc->Lbright_rc, 1000,0,500,2, 
		"Brightness", update_sliders, update_sliders, dc, 1, 
		XmHORIZONTAL, wargs, n);
	XtAddCallback(dc->sliders[LITE_BGT],XmNdragCallback,
			update_lightvals,dc);	
	XtAddCallback(dc->sliders[LITE_BGT],XmNvalueChangedCallback,
			update_lightvals,dc);	

	n = 0;
	SetPositionArgs(wargs, &n, 21, -1, 46, 60, XmATTACH_NONE);
	dc->slider_txt[LITE_BGT] = make_text(dc->panels[LIGHT].form, "", 
		5, "bright", update_sliders, dc, wargs, n);



	/*-- Ambient slider and text --*/
	n = 0;
	SetPositionArgs(wargs, &n, 32, -1, 2, 44, XmATTACH_NONE);
	XtSetArg (wargs[n], XmNorientation, XmHORIZONTAL); n++;
	dc->Lambient_rc = XtCreateManagedWidget("b_rc", xmRowColumnWidgetClass,
		dc->panels[LIGHT].form, wargs, n);

	n = 0;
	XtSetArg(wargs[n],XmNscaleHeight,20); n++;
	XtSetArg(wargs[n],XmNscaleWidth,115); n++;
	dc->sliders[LITE_AMB] = make_slider(dc->Lambient_rc, 1000,0,500,2, 
		"Ambient", update_sliders,update_sliders,dc, 1,
		XmHORIZONTAL, wargs, n); 
	XtAddCallback(dc->sliders[LITE_AMB],XmNdragCallback,
			update_lightvals,dc);	
	XtAddCallback(dc->sliders[LITE_AMB],XmNvalueChangedCallback,
			update_lightvals,dc);	

	n = 0;
	SetPositionArgs(wargs, &n, 32, -1, 46, 60, XmATTACH_NONE);
	dc->slider_txt[LITE_AMB] = make_text(dc->panels[LIGHT].form, "", 
		5, "ambient", update_sliders, dc, wargs, n);


	/*-- Red slider and text --*/
	n = 0;
	SetPositionArgs(wargs, &n, 44, -1, 2, 44, XmATTACH_NONE);
	XtSetArg (wargs[n], XmNorientation, XmHORIZONTAL); n++;
	dc->Lr_rc = XtCreateManagedWidget("r_rc", xmRowColumnWidgetClass, 
		dc->panels[LIGHT].form, wargs, n);

	n = 0;
	XtSetArg(wargs[n],XmNscaleHeight,20); n++;
	XtSetArg(wargs[n],XmNscaleWidth,115); n++;
	dc->sliders[LITE_RED] = make_slider(dc->Lr_rc, 1000,0,500,2, "Red", 
		update_sliders, update_sliders, dc, 1,
		XmHORIZONTAL, wargs, n); 
	XtAddCallback(dc->sliders[LITE_RED],XmNdragCallback,
			update_lightvals,dc);	
	XtAddCallback(dc->sliders[LITE_RED],XmNvalueChangedCallback,
			update_lightvals,dc);	
	    
	n = 0;
	SetPositionArgs(wargs, &n, 45, -1, 46, -1, XmATTACH_NONE);
	dc->slider_txt[LITE_RED] = make_text(dc->panels[LIGHT].form, "", 
		5, "bright", update_sliders, dc, wargs, n);

	/*-- Green slider and text --*/
	n = 0;
	SetPositionArgs(wargs, &n, 55, -1, 2, 44, XmATTACH_NONE);
	XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
	dc->Lg_rc = XtCreateManagedWidget("g_rc", xmRowColumnWidgetClass, 
		dc->panels[LIGHT].form, wargs, n);

	n = 0;
	XtSetArg(wargs[n],XmNscaleHeight,20); n++;
	XtSetArg(wargs[n],XmNscaleWidth,115); n++;
	dc->sliders[LITE_GRN] = make_slider(dc->Lg_rc, 1000,0,500,2, "Green", 
		update_sliders, update_sliders, dc, 1,
		XmHORIZONTAL, wargs, n); 
	XtAddCallback(dc->sliders[LITE_GRN],XmNdragCallback,
			update_lightvals,dc);	
	XtAddCallback(dc->sliders[LITE_GRN],XmNvalueChangedCallback,
			update_lightvals,dc);	

	n = 0;
	SetPositionArgs(wargs, &n, 55, -1, 46, -1, XmATTACH_NONE);
	dc->slider_txt[LITE_GRN] = make_text(dc->panels[LIGHT].form, "", 5, 
		"bright", update_sliders, dc, wargs, n);

	/*-- Blue slider and text --*/
	n = 0;
	SetPositionArgs(wargs, &n, 65, -1, 2, 44, XmATTACH_NONE);
	XtSetArg (wargs[n], XmNorientation, XmHORIZONTAL); n++;
	dc->Lb_rc = XtCreateManagedWidget("b_rc", xmRowColumnWidgetClass, 
		dc->panels[LIGHT].form, wargs, n);

	n = 0;
	XtSetArg(wargs[n],XmNscaleHeight,20); n++;
	XtSetArg(wargs[n],XmNscaleWidth,115); n++;
	dc->sliders[LITE_BLU] = make_slider(dc->Lb_rc, 1000,0,500,2, "Blue", 
		update_sliders, update_sliders, dc, 1,
		XmHORIZONTAL, wargs, n); 
	XtAddCallback(dc->sliders[LITE_BLU],XmNdragCallback,
			update_lightvals,dc);	
	XtAddCallback(dc->sliders[LITE_BLU],XmNvalueChangedCallback,
			update_lightvals,dc);	

	n = 0;
	SetPositionArgs(wargs, &n, 65, -1, 46, -1, XmATTACH_NONE);
	dc->slider_txt[LITE_BLU] = make_text(dc->panels[LIGHT].form, "", 5, 
		"blue", update_sliders, dc, wargs, n);


	/*------  the orientation of this slider will be vertical ------*/
	n = 0;
	SetPositionArgs(wargs, &n, 46, 75, 68, 75, NULL);
	XtSetArg (wargs[n], XmNorientation, XmVERTICAL); n++;
	dc->Lheight_rc = XtCreateManagedWidget("b_rc", xmRowColumnWidgetClass, 
		dc->panels[LIGHT].form, wargs, n);

	n = 0;
	XtSetArg(wargs[n],XmNscaleHeight, 140); n++;
	XtSetArg(wargs[n],XmNscaleWidth, 20); n++;
	dc->sliders[LITE_HGT] = make_slider(dc->Lheight_rc, 1000,0,500,2, "", 
		update_sliders, update_sliders,dc, 0,
		XmVERTICAL, wargs, n); 
	XtAddCallback(dc->sliders[LITE_HGT],XmNdragCallback,
			position_light_z,dc);	
	XtAddCallback(dc->sliders[LITE_HGT],XmNvalueChangedCallback,
			position_light_z,dc);	

	n = 0;
	SetPositionArgs(wargs, &n, 47, -1, 76, -1, XmATTACH_NONE);
	XtCreateManagedWidget("Height", xmLabelGadgetClass, dc->panels[LIGHT].
		form, wargs, n);

	n = 0;
	SetPositionArgs(wargs, &n, 53, -1, 76, -1, XmATTACH_NONE);
	dc->slider_txt[LITE_HGT] = make_text(dc->panels[LIGHT].form, "", 5, 
		"height", update_sliders, dc, wargs, n);


	/*------ make the xy positioner of  the light ------*/
	n = 0; 
	SetPositionArgs(wargs, &n, 17, 45, 63, 98, NULL);
	dc->Lxy_pos = make_position(dc->panels[LIGHT].form,"xy-position",
		  trackmouse, trackmouse, NULL,lgtexp,dc,wargs,n);
	XtAddEventHandler(dc->Lxy_pos, ButtonPressMask | Button1MotionMask,
			    FALSE, position_light_xy, dc);

	n = 0;
	SetPositionArgs(wargs, &n, 10, -1, 63, 98, XmATTACH_NONE);
	XtCreateManagedWidget("X-Y Position", xmLabelGadgetClass, 
	    dc->panels[LIGHT].form, wargs, n);

#ifdef OLD
#endif /* OLD */
    
	/*
	n = 0;
	XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNtopPosition,25); n++;
	XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNbottomPosition,35); n++;
	XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNleftPosition,2); n++;
	XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNrightPosition,45); n++;
	XtSetArg (wargs[n], XmNorientation, XmHORIZONTAL); n++;
	dc->Lshiney_rc = XtCreateManagedWidget("s_rc", xmRowColumnWidgetClass,
		dc->panels[LIGHT].form, wargs, n);

	n = 0;
	XtSetArg(wargs[n],XmNscaleHeight,20); n++;
	XtSetArg(wargs[n],XmNscaleWidth,160); n++;
	dc->sliders[LITE_SHN] = make_slider(dc->Lshiney_rc, 1000,0,500,2, 
		"Shininess", update_sliders,update_sliders,dc, 1,
		XmHORIZONTAL, wargs, n); 

	n = 0;
	XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNtopPosition,25); n++;
	XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_NONE); n++;
	XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNleftPosition,46); n++;
	XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_NONE); n++;
	dc->slider_txt[LITE_SHN] = make_text(dc->panels[LIGHT].form, "", 5, 
		"Shine", update_sliders, dc, wargs, n);
	-- Shininess slider and text --*/

	closeb(LIGHT,dc,0);
    /*
	XtAddCallback(dc->panels[LIGHT].close,XmNactivateCallback,close_me,dc);	
   */ 
	set_light_sliders(dc);
        inform(dc, "Done");
        first = 0;

    }
    else if(dc->panels[LIGHT].im_already_open){
	dc->constant = LIGHT;
	pops(w, dc, LIGHT);
	return;
    }

    else {
        inform(dc, "Calculating Points");
        reshow(dc,LIGHT, 1);
	inform(dc, "Done");
    }


}


#define NUM_LSLIDERS 6

int 
set_light_sliders (data_cell *dc)
{
int i;
Arg wargs[2];
int sl_max, sl_min, sl_range, ival, sl_id[NUM_LSLIDERS];
char str[20];

    sl_id[0] = LITE_BGT ;
    sl_id[1] = LITE_AMB ;
    sl_id[2] = LITE_RED ;
    sl_id[3] = LITE_GRN ;
    sl_id[4] = LITE_BLU ;
    sl_id[5] = LITE_HGT ;

    for(i=0; i < NUM_LSLIDERS; i++){
	if(XtIsManaged(dc->sliders[sl_id[i]])){
	    XtSetArg (wargs[0], XmNmaximum, &sl_max);	    
	    XtSetArg (wargs[1], XmNminimum, &sl_min);	    
	    XtGetValues(dc->sliders[sl_id[i]], wargs, 2);
	    sl_range = sl_max - sl_min;

	    ival = (int)(sl_min + dc->slider_values[sl_id[i]] * sl_range);
	    XmScaleSetValue(dc->sliders[sl_id[i]], ival); 

	    sprintf (str, "%1.3f", SLIDER_VAL_REAL(dc, sl_id[i]));
	    XmTextSetString(dc->slider_txt[sl_id[i]], str);
	}
    }

}
