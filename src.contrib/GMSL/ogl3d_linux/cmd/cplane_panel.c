
/* cplane_panel:
** creation routine used for panel that sets clipping planes 
*/

#include  "interface.h"

/*************************************
* the cplane panel creation routine  **
*************************************/

#define DECIM 0

static void update_xytrans();
static Widget XY_tran, X_txt, Y_txt;
extern Widget make_simple_pupcheckbox();


static float Cp_trans[MAX_CPLANES][3], Cp_rot[MAX_CPLANES][3];
static char *Cp_labels[MAX_CPLANES];

int 
init_cp_labels (void)
{
int i;
char tmp[32];

    for(i=0; i<MAX_CPLANES; i++){
	sprintf(tmp, "Plane %d", i);
	Cp_labels[i] = (char *)malloc ((strlen (tmp) + 32) * sizeof (char));
	strcpy(Cp_labels[i], tmp);
    }

}


/***********************************************************************/
void 
fcolor_above (Widget w, data_cell *dc, caddr_t *call_data)
{

    GS_set_fencecolor(FC_ABOVE);

}

/***********************************************************************/
void 
fcolor_below (Widget w, data_cell *dc, caddr_t *call_data)
{

    GS_set_fencecolor(FC_BELOW);

}

/***********************************************************************/
void 
fcolor_blend (Widget w, data_cell *dc, caddr_t *call_data)
{

    GS_set_fencecolor(FC_BLEND);

}

/***********************************************************************/
void 
fcolor_grey (Widget w, data_cell *dc, caddr_t *call_data)
{

    GS_set_fencecolor(FC_GREY);

}

/***********************************************************************/
void 
fcolor_off (Widget w, data_cell *dc, caddr_t *call_data)
{

    GS_set_fencecolor(FC_OFF);

}

/***********************************************************************/
void 
cplane_rotate (Widget w, data_cell *dc, caddr_t *call_data)
{
    Cp_rot[dc->CurCplane][Y] = (float)SLIDER_VAL_REAL(dc, CPL_TILT);
    Cp_rot[dc->CurCplane][Z] = (float)SLIDER_VAL_REAL(dc, CPL_ROT);
    Cp_rot[dc->CurCplane][X] = 0.0; 
    GS_set_cplane_rot(dc->CurCplane, Cp_rot[dc->CurCplane][X], 
		    Cp_rot[dc->CurCplane][Y], Cp_rot[dc->CurCplane][Z]);
    cplane_draw(dc->CurCplane, dc);

}
/***********************************************************************/
void 
cplane_translate (Widget w, data_cell *dc, caddr_t *call_data)
{

    GS_set_cplane_trans(dc->CurCplane, Cp_trans[dc->CurCplane][X], 
		    Cp_trans[dc->CurCplane][Y], Cp_trans[dc->CurCplane][Z]);
    cplane_draw(dc->CurCplane, dc);

}

/****************/
static void 
set_ztrans (Widget w, data_cell *dc, caddr_t *call_data)
{
    Cp_trans[dc->CurCplane][Z] = (float)SLIDER_VAL_REAL(dc, CPL_ZTR);
    cplane_translate(w, dc, NULL);

}


/****************/
/* callback for X_txt & Y_txt */
static void 
update_xytrans (Widget w, data_cell *dc, caddr_t *call_data)
{
char *ctmp;
float ftmp, cx, cy;

    ctmp = XmTextGetString(w);
    ftmp = atof(ctmp);

    if(w == X_txt)
	Cp_trans[dc->CurCplane][X] = ftmp;
    else if(w == Y_txt)
	Cp_trans[dc->CurCplane][Y] = ftmp;

    cx = Cp_trans[dc->CurCplane][X]/dc->XYrange;
    cy = Cp_trans[dc->CurCplane][Y]/dc->XYrange;
    set_crosshair(cx, cy);
    /* check & constrain? */

    draw_crosshair(XY_tran, dc, NULL);

    cplane_translate(w, dc, NULL);

}

/****************/
static void 
update_xytxt (data_cell *dc)
{
char str[20];

    sprintf (str, "%d", (int)Cp_trans[dc->CurCplane][X]);
    XmTextSetString(X_txt, str);
    sprintf (str, "%d", (int)Cp_trans[dc->CurCplane][Y]);
    XmTextSetString(Y_txt, str);
}

/****************/
/* event handler for xy_translation widget */
static void 
set_xytrans (Widget w, data_cell *dc, XEvent *event)
{
int left, top, right, bottom;
unsigned int width, height, bwidth, depth;
Window win;
int xc, yc;
    
    /* assumes w is the xytrans widget box */
    XGetGeometry(dc->dpy, XtWindow(w), &win, &left, &top,
		    &width, &height, &bwidth, &depth);

    right = left + width;
    bottom = top + height;

    xc = left + width/2;
    yc = top + height/2;
    
    Cp_trans[dc->CurCplane][X] = (float)(dc->x - xc)/width * dc->XYrange;
    Cp_trans[dc->CurCplane][Y] = (float)(yc - dc->y)/height * dc->XYrange;

    update_xytxt(dc);

    cplane_translate(w, dc, NULL);
}

/****************/
static void 
cp_popact(w, dc, call_data)
    Widget w;
    data_cell *dc;
    XmPushButtonCallbackStruct *call_data;
{
Arg wargs[30];
int n=0;
static int first = 1;

    if(first){
	first = 0;
	dc->Wcp_act = make_simple_pupcheckbox(dc->panels[CPLANE].form, 
			"wcp_act", "", Cp_labels, MAX_CPLANES, dc->CurCplane,
			set_active_cplanes, wargs, n);
    }

    XmMenuPosition(XtParent(dc->Wcp_act), call_data->event);
    XtManageChild(dc->Wcp_act);

}


/****************/
static void 
cp_alloff (Widget w, data_cell *dc, caddr_t call_data)
{
int i;

    for(i=0; i<MAX_CPLANES; i++){
	GS_unset_cplane(i);
	dc->Cp_on[i] = 0;
    }

    /* TODO: fix active widgets */
    quick_draw(dc);
}

/****************/
static void 
do_reset (Widget w, data_cell *dc, caddr_t call_data)
{

    Cp_trans[dc->CurCplane][X] = Cp_trans[dc->CurCplane][Y] =
			      Cp_trans[dc->CurCplane][Z] = 0.0;
    Cp_rot[dc->CurCplane][X] = 0.0; 
    Cp_rot[dc->CurCplane][Y] = Cp_rot[dc->CurCplane][Z] = 180.0;

    set_cp_widgets(dc);

    cplane_translate(w, dc, NULL);
    cplane_rotate(w, dc, NULL);
}

/****************/
static void 
set_ranges (data_cell *dc)
{
float cx, cy, zmin, zmax, val;


    dc->slider_min[CPL_ZTR] = -(dc->Zrange);
    dc->slider_max[CPL_ZTR] = dc->Zrange;
    val = UNIT_OF(dc->Zrange, -(dc->Zrange), Cp_trans[dc->CurCplane][Z]);
    if(val < 0) val = 0.;
    else if(val > 1) val = 1.;
    dc->slider_values[CPL_ZTR] = val;

    val = UNIT_OF(360., 0., Cp_rot[dc->CurCplane][Y]);  /* TILT */
    if(val < 0) val = 0.;
    else if(val > 1) val = 1.;
    dc->slider_values[CPL_TILT] = val;

    val = UNIT_OF(360., 0., Cp_rot[dc->CurCplane][Z]);  /* ROT */
    if(val < 0) val = 0.;
    else if(val > 1) val = 1.;
    dc->slider_values[CPL_ROT] = val;

    cx = Cp_trans[dc->CurCplane][X]/dc->XYrange;
    cy = Cp_trans[dc->CurCplane][Y]/dc->XYrange;
    set_crosshair(cx, cy);
    /* check & constrain? */

}


/****************/
void 
set_cp_widgets (data_cell *dc)
{
Arg wargs[2];
int sl_max, sl_min, sl_range, ival;
char str[20];

    set_ranges(dc);
    set_slider_txt(dc, CPL_TILT);
    set_slider_txt(dc, CPL_ROT);
    set_slider_txt(dc, CPL_ZTR);
	
    update_xytxt(dc);

    draw_crosshair(XY_tran, dc, NULL);

}


/****************/
void 
cplane_panel (Widget w, data_cell *dc, caddr_t client_data)
{
    int n;
    Arg wargs[30];
    static int first = 1;
    Widget tw, trc, reset;

    if(first) {

	inform(dc,"Calculating Positions");
	dc->panels[CPLANE].im_already_open = 1;
	dc->panels[CPLANE].name = XmStringCreateSimple("Cutting Planes Panel");
	check_space(dc, CPLANE, 0);

	dc->CurCplane = 0;
	for(n=0; n < MAX_CPLANES; n++){
	    Cp_rot[n][Y] = Cp_rot[n][Z] = 180.0;
	    Cp_rot[n][X] = 0.0;
	    Cp_trans[n][X] = Cp_trans[n][Y] = Cp_trans[n][Z] = 0.0;
	}

	n = 0;
	SetPositionArgs(wargs, &n, dc->here.toph, 80,
						-1, -1, XmATTACH_FORM);
	dc->panels[CPLANE].form = XtCreateManagedWidget("cplform",
		xmFormWidgetClass, dc->form_for_aux_control_panel, wargs,n);

	make_title(CPLANE,dc, 0);
	closeb(CPLANE,dc,0);		

/* make widgets */



	/* -- Z translation Slider -- */
	n = 0;
	SetPositionArgs(wargs, &n, 11, 55, 3, 16, XmATTACH_NONE); n++;
	trc = XtCreateManagedWidget("cp_zrc", xmRowColumnWidgetClass,
		    dc->panels[CPLANE].form, wargs, n);
	
	n = 0;
	XtSetArg(wargs[n],XmNscaleHeight,120); n++;
	XtSetArg(wargs[n],XmNscaleWidth,15); n++;
	dc->sliders[CPL_ZTR] = make_slider(trc, 10000, 0, 0, DECIM,
		    "Z-Separation",update_sliders,update_sliders, dc,0,
		    XmVERTICAL,wargs,n);
	XtAddCallback(dc->sliders[CPL_ZTR], XmNdragCallback, set_ztrans, dc);
	XtAddCallback(dc->sliders[CPL_ZTR], XmNvalueChangedCallback, 
		    set_ztrans, dc);
	
	n = 0;
	SetPositionArgs(wargs, &n, 58, -1, 2, 17, XmATTACH_NONE); n++;
	XtCreateManagedWidget("Z:", xmLabelGadgetClass, dc->panels[CPLANE].form, wargs, n);
	

	n = 0;
	SetPositionArgs(wargs, &n, 65, -1, 2, 18, XmATTACH_NONE); n++;
	dc->slider_txt[CPL_ZTR] = make_text(dc->panels[CPLANE].form, "", 8,"ztr_txt", 
		    update_sliders, dc,wargs,n);
	XtAddCallback(dc->slider_txt[CPL_ZTR], XmNactivateCallback,
		    set_ztrans, dc);


	/* XY positioner */
	n = 0;
	SetPositionArgs(wargs, &n, 11, -1, 15, 52, XmATTACH_NONE);
	XtSetArg(wargs[n], XmNwidth, 100); n++;
	XtSetArg(wargs[n], XmNheight, 100); n++;
	XY_tran = make_position (dc->panels[CPLANE].form, "cp_xytran", 
			trackmouse2, trackmouse2, NULL, exp2, dc, wargs, n);
	XtAddEventHandler(XY_tran, ButtonPressMask | Button1MotionMask,
			FALSE, set_xytrans, dc);

	n = 0;
	SetPositionArgs(wargs, &n, 50, 58, 21, 27, XmATTACH_NONE);
	XtCreateManagedWidget("X:", xmLabelGadgetClass,
		    dc->panels[CPLANE].form, wargs, n);
	
	n = 0;
	SetPositionArgs(wargs, &n, 50, 58, 28, 43, XmATTACH_NONE);
	X_txt = make_text(dc->panels[CPLANE].form, "", 8,"cp_xtr_txt", 
		    update_xytrans, dc,wargs,n);
	
	n = 0;
	SetPositionArgs(wargs, &n, 60, 68, 21, 27, XmATTACH_NONE);
	XtCreateManagedWidget("Y:", xmLabelGadgetClass,
		    dc->panels[CPLANE].form, wargs, n);
	
	n = 0;
	SetPositionArgs(wargs, &n, 60, 68, 28, 43, XmATTACH_NONE);
	Y_txt = make_text(dc->panels[CPLANE].form, "", 8,"cp_ytr_txt", 
		    update_xytrans, dc,wargs,n);





	/* -- Option menu for selecting current plane -- */
	n = 0;
	SetPositionArgs(wargs, &n, 10, -1, 54, -1, XmATTACH_NONE);
	tw = make_simple_options(dc->panels[CPLANE].form, "wcur_cp", 
		    "Current:", Cp_labels, MAX_CPLANES, dc->CurCplane, 
		    set_cur_cplane, wargs, n);
	

	/* -- Rotate & Tilt Sliders (dials would be better) -- */
	n = 0;
	SetPositionArgs(wargs, &n, 34, -1, 52, 73, XmATTACH_NONE);
	trc = XtCreateManagedWidget("cp_rotrc", xmRowColumnWidgetClass,
		    dc->panels[CPLANE].form, wargs, n);
	
	n = 0;
	XtSetArg(wargs[n],XmNscaleHeight,75); n++;
	XtSetArg(wargs[n],XmNscaleWidth,20); n++;
	XtSetArg(wargs[n],XmNscaleMultiple,10); n++;
	dc->sliders[CPL_ROT] = make_slider(trc, 10000, 0, 0, 1,
		    "cp_rot",update_sliders,update_sliders, dc,0,
		    XmVERTICAL,wargs,n);
	XtAddCallback(dc->sliders[CPL_ROT], XmNdragCallback, cplane_rotate, dc);
	XtAddCallback(dc->sliders[CPL_ROT], XmNvalueChangedCallback, 
		    cplane_rotate, dc);
	
	n = 0;
	XtCreateManagedWidget("Rotate", xmLabelGadgetClass, trc, wargs, n);
	
	n = 0;
	dc->slider_txt[CPL_ROT] = make_text(trc,
		    "", 7,"cp_rot_txt", update_sliders, dc,wargs,n);
	XtAddCallback(dc->slider_txt[CPL_ROT], XmNactivateCallback,
		    cplane_rotate, dc);


	n = 0;
	SetPositionArgs(wargs, &n, 34, -1, 75, -1, XmATTACH_NONE);
	trc = XtCreateManagedWidget("cp_tiltrc", xmRowColumnWidgetClass,
		    dc->panels[CPLANE].form, wargs, n);
	
	n = 0;
	XtSetArg(wargs[n],XmNscaleHeight,75); n++;
	XtSetArg(wargs[n],XmNscaleWidth,20); n++;
	XtSetArg(wargs[n],XmNscaleMultiple,10); n++;
	dc->sliders[CPL_TILT] = make_slider(trc, 10000, 0, 0, 1,
		    "cp_tilt",update_sliders,update_sliders, dc,0,
		    XmVERTICAL,wargs,n);
	XtAddCallback(dc->sliders[CPL_TILT],XmNdragCallback,cplane_rotate, dc);
	XtAddCallback(dc->sliders[CPL_TILT], XmNvalueChangedCallback, 
		    cplane_rotate, dc);
	
	n = 0;
	XtCreateManagedWidget("Tilt", xmLabelGadgetClass, trc, wargs, n);
	
	n = 0;
	dc->slider_txt[CPL_TILT] = make_text(trc,
		    "", 7,"cp_tilt_txt", update_sliders, dc,wargs,n);
	XtAddCallback(dc->slider_txt[CPL_TILT], XmNactivateCallback,
		    cplane_rotate, dc);

	n = 0;
        SetPositionArgs(wargs, &n, 24,33,54,-1, XmATTACH_FORM);
        XtSetArg(wargs[n],XmNspacing,0); n++;
        XtSetArg(wargs[n],XmNorientation,XmHORIZONTAL); n++;
        XtSetArg(wargs[n],XmNradioBehavior,TRUE); n++;	
	trc = XtCreateManagedWidget("fencemode_rc",
		xmRowColumnWidgetClass, dc->panels[CPLANE].form, wargs, n);

	/* fence color above */
	n = 0;
    	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	tw = XtCreateManagedWidget("T", xmToggleButtonWidgetClass, 
		    trc, wargs, n);
	XtAddCallback(tw,XmNdisarmCallback,fcolor_above,dc);

	/* fence color below */
	n = 0;
    	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	tw = XtCreateManagedWidget("B", xmToggleButtonWidgetClass, 
		    trc, wargs, n);
	XtAddCallback(tw,XmNdisarmCallback,fcolor_below,dc);

	/* fence color blend */
	GS_set_fencecolor(FC_BLEND);
	n = 0;
    	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	XtSetArg(wargs[n],XmNset,TRUE); n++;
	tw = XtCreateManagedWidget("BL", xmToggleButtonWidgetClass, 
		    trc, wargs, n);
	XtAddCallback(tw,XmNdisarmCallback,fcolor_blend,dc);

	/* fence color grey */
	n = 0;
	tw = XtCreateManagedWidget("GR", xmToggleButtonWidgetClass, 
		    trc, wargs, n);
	XtAddCallback(tw,XmNdisarmCallback,fcolor_grey,dc);

	/* fence color off */
	n = 0;
	tw = XtCreateManagedWidget("N", xmToggleButtonWidgetClass, 
		    trc, wargs, n);
	XtAddCallback(tw,XmNdisarmCallback,fcolor_off,dc);







	/* all off */
	n = 0;
	SetPositionArgs(wargs, &n, -1, 98, 55, -1, XmATTACH_NONE);
	tw = XtCreateManagedWidget("All Off", xmPushButtonWidgetClass, 
		    dc->panels[CPLANE].form, wargs, n);
	XtAddCallback(tw,XmNactivateCallback,cp_alloff,dc);
	

	/* Reset */
	n = 0;
	SetPositionArgs(wargs, &n, -1, 98, 2, -1, XmATTACH_NONE);
	reset = XtCreateManagedWidget("Reset", xmPushButtonWidgetClass, 
		    dc->panels[CPLANE].form, wargs, n);
	XtAddCallback(reset,XmNactivateCallback,do_reset,dc);
	
	
	inform(dc,"Done");
	first = 0;
	GS_set_cplane(dc->CurCplane);  
	dc->Cp_on[dc->CurCplane] = 1;
	do_reset(reset, dc, NULL);
    /* TODO: set active? */













}









/*---------------------------------------------------------------------
this section checks to see if the panel is already open and acts    --
accordingly                                                        --
---------------------------------------------------------------------*/
    else if(dc->panels[CPLANE].im_already_open){
	dc->constant = CPLANE;
	pops(w, dc, CPLANE);
	return;
    }

    else {
	inform(dc,"Calculating Positions");
        reshow(dc,CPLANE, 1); 
        inform(dc,"Done");
	GS_set_cplane(dc->CurCplane);  
	dc->Cp_on[dc->CurCplane] = 1;
	set_cp_widgets(dc);
    }
    /* TODO: and set active & current widgets - NOT! (they remember) */

}



