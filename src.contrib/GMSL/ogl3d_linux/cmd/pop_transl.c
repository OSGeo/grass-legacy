/* pop_transl:
** controls pop up dialog for translating surfaces
*/


#include "interface.h"

#define DECIM 0 
/* This is used for text widgets 
   make this a variable that gets set according to whether lat-lon or 
   UTM - don't need decimal accuracy with UTM */

Widget XY_tran, X_txt, Y_txt;
float Cx, Cy;  /* crosshair location used for drawing, in range -.5 to .5 */

static void do_reset();
static void set_ranges();
static void set_xytrans();
static void set_ztrans();
static void update_xytrans();
static void update_xytxt();

/********************* utilities ****************/
/****************/
/* event handler for xy_translation widget */
static void 
set_xytrans (Widget w, data_cell *dc, XEvent *event)
{

int left, top, right, bottom;
unsigned int width, height, bwidth, depth;
Window win;
int xc, yc;
surf_dm *dm;
    
    /* assumes w is the xytrans widget box */
    XGetGeometry(dc->dpy, XtWindow(w), &win, &left, &top,
		    &width, &height, &bwidth, &depth);

    right = left + width;
    bottom = top + height;

    xc = left + width/2;
    yc = top + height/2;
    
    dm = &(dc->Surf_Settings[dc->CurSurf]);
    dm->xtrans = (float)(dc->x - xc)/width * dc->XYrange;
    dm->ytrans = (float)(yc - dc->y)/height * dc->XYrange;

    update_xytxt(dc);

    surf_translate(w, dc, NULL);
}

/****************/
static void 
set_ztrans (Widget w, data_cell *dc, caddr_t *call_data)
{
surf_dm *dm;

    dm = &(dc->Surf_Settings[dc->CurSurf]);
/*
fprintf(stderr,"%f\n", dm->ztrans);
*/
    dm->ztrans = (float)SLIDER_VAL_REAL(dc, SURF_ZTR);
/*
fprintf(stderr,"(%f)\n", dm->ztrans);
*/

    surf_translate(w, dc, NULL);

}

/****************/
void 
set_crosshair (double cx, double cy)
{
    Cx = cx;
    Cy = cy;
}

/****************/
/* was drawpuck */
void 
draw_crosshair (Widget w, data_cell *dc, caddr_t *call_data)
{
int left, top, right, bottom, xc, yc, x0, y0;
unsigned int width, height, bwidth, depth;
Window win;

    XGetGeometry(dc->dpy, XtWindow(w), &win, &left, &top,
		    &width, &height, &bwidth, &depth);
    XClearArea (dc->dpy, XtWindow (w), 0, 0, 0, 0, FALSE);

    right = left + width;
    bottom = top + height;

    xc = left + width/2;
    yc = top + height/2;

    x0 = Cx * width + xc;
    y0 = -Cy * height + yc;

    XDrawLine(dc->dpy, XtWindow(w), dc->gc,x0,top,x0,bottom);
    XDrawLine(dc->dpy, XtWindow(w), dc->gc,left,y0,right,y0);

}

/****************/
void 
trackmouse2 (Widget w, data_cell *dc, XEvent *event)
{
static XEvent event2;
int left, top, right, bottom;
unsigned int width, height, bwidth, depth;
Window win;
int xc, yc;
surf_dm *dm;
    
    if(event) {
	dc->x = event2.xbutton.x = event->xbutton.x;
	dc->y = event2.xbutton.y = event->xbutton.y;
	/* assumes w is the xytrans widget box */
	XGetGeometry(dc->dpy, XtWindow(w), &win, &left, &top,
			&width, &height, &bwidth, &depth);

	right = left + width;
	bottom = top + height;

	xc = left + width/2;
	yc = top + height/2;

	Cx = (float)(dc->x - xc)/width;
	Cy = (float)(yc - dc->y)/height;
    }

    else {  /* still need this ? */
	dc->x = event2.xbutton.x;
	dc->y = event2.xbutton.y;
    }
    
    draw_crosshair (w, dc, NULL);

}

/****************/
/* was lgtexp */
void 
exp2 (Widget w, data_cell *dc, caddr_t call_data)
{
    trackmouse2(w, dc, NULL);
}

/****************/
static void 
do_reset (Widget w, data_cell *dc, caddr_t call_data)
{
surf_dm *dm;

    dm = &(dc->Surf_Settings[dc->CurSurf]);
    dm->xtrans = dm->ytrans = dm->ztrans = 0.0;

    set_trans_widgets(dc);
    surf_translate(w, dc, NULL);
}

/****************/
static void 
set_ranges (data_cell *dc)
{
float zmin, zmax, val;
surf_dm *dm;

    dm = &(dc->Surf_Settings[dc->CurSurf]);

    dc->slider_min[SURF_ZTR] = -(dc->Zrange);
    dc->slider_max[SURF_ZTR] = dc->Zrange;
    val = UNIT_OF(dc->Zrange, -(dc->Zrange), dm->ztrans);
    if(val < 0) val = 0.;
    else if(val > 1) val = 1.;
    dc->slider_values[SURF_ZTR] = val;

    Cx = dm->xtrans/dc->XYrange;
    Cy = dm->ytrans/dc->XYrange;
    /* check & constrain? */

}


/****************/
static void 
update_xytxt (data_cell *dc)
{
surf_dm *dm;
char str[20];

    dm = &(dc->Surf_Settings[dc->CurSurf]);

    sprintf (str, "%d", (int)dm->xtrans);
    XmTextSetString(X_txt, str);
    sprintf (str, "%d", (int)dm->ytrans);
    XmTextSetString(Y_txt, str);
}


/****************/
/* call when translate window is popped OR when current surface changes */
void 
set_trans_widgets (data_cell *dc)
{
Arg wargs[2];
int sl_max, sl_min, sl_range, ival;
char str[20];

    set_ranges(dc);

    if(XtIsManaged(dc->sliders[SURF_ZTR])){
	XtSetArg (wargs[0], XmNmaximum, &sl_max);	    
	XtSetArg (wargs[1], XmNminimum, &sl_min);	    
	XtGetValues(dc->sliders[SURF_ZTR], wargs, 2);
	sl_range = sl_max - sl_min;

	ival = (int)(sl_min + dc->slider_values[SURF_ZTR] * sl_range);
	XmScaleSetValue(dc->sliders[SURF_ZTR], ival); 

	sprintf (str, "%d", (int)SLIDER_VAL_REAL(dc, SURF_ZTR));
	XmTextSetString(dc->slider_txt[SURF_ZTR], str);
	
	update_xytxt(dc);

	draw_crosshair(XY_tran, dc, NULL);
    }


}

/****************/
/* callback for X_txt & Y_txt */
static void 
update_xytrans (Widget w, data_cell *dc, caddr_t *call_data)
{
char *ctmp;
float ftmp;
surf_dm *dm;


    dm = &(dc->Surf_Settings[dc->CurSurf]);
    ctmp = XmTextGetString(w);
    ftmp = atof(ctmp);

    if(w == X_txt)
	dm->xtrans = ftmp;
    else if(w == Y_txt)
	dm->ytrans = ftmp;

    Cx = dm->xtrans/dc->XYrange;
    Cy = dm->ytrans/dc->XYrange;
    /* check & constrain? */

    draw_crosshair(XY_tran, dc, NULL);

    surf_translate(w, dc, NULL);

}


/**********************************************************************/


void 
pop_transl (Widget w, data_cell *dc, caddr_t client_data)
{
Widget trc, reset, ok;
int n;
Arg wargs[15];
char str[80];
void (*cb1)(), (*cb2)();
static int first = 1;


    if(first){

	n = 0;
    /*
	XtSetArg(wargs[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;
    */
	XtSetArg(wargs[n],XmNautoUnmanage, FALSE); n++;
	XtSetArg(wargs[n],XmNdialogTitle, 
			XmStringCreateSimple("Move Surface")); n++;

/*  works good, but not centered over control panel.(centered over button) */
	dc->Wtrans_pop = XmCreateFormDialog(w,
			"pop_transl", wargs, n);
	
	n = 0;
	SetPositionArgs(wargs, &n, 5, -1, 35, 90, XmATTACH_NONE);
	XtSetArg(wargs[n], XmNwidth, 140); n++;
	XtSetArg(wargs[n], XmNheight, 140); n++;
	XY_tran = make_position (dc->Wtrans_pop, "xytran", trackmouse2, 
			trackmouse2, NULL, exp2, dc, wargs, n);
	XtAddEventHandler(XY_tran, ButtonPressMask | Button1MotionMask,
			FALSE, set_xytrans, dc);

	/* -- Z separation Slider -- */
	n = 0;
	SetPositionArgs(wargs, &n, 5, 70, 10, 30, NULL);
	trc = XtCreateManagedWidget("zseprc", xmRowColumnWidgetClass,
		    dc->Wtrans_pop, wargs, n);
	
	n = 0;
	XtSetArg(wargs[n],XmNscaleHeight,150); n++;
	XtSetArg(wargs[n],XmNscaleWidth,35); n++;
	XtSetArg(wargs[n],XmNscaleMultiple,10); n++;
	dc->sliders[SURF_ZTR] = make_slider(trc, 10000, 0, 0, DECIM,
		    "Z-Separation",update_sliders,update_sliders, dc,0,
		    XmVERTICAL,wargs,n);
	XtAddCallback(dc->sliders[SURF_ZTR], XmNdragCallback, set_ztrans, dc);
	XtAddCallback(dc->sliders[SURF_ZTR], XmNvalueChangedCallback, 
		    set_ztrans, dc);
	
	n = 0;
	SetPositionArgs(wargs, &n, 75, -1, 5, -1, XmATTACH_NONE);
	XtCreateManagedWidget("Z:", xmLabelGadgetClass,
		    dc->Wtrans_pop, wargs, n);
	
	n = 0;
	SetPositionArgs(wargs, &n, 72, -1, 12, -1, XmATTACH_NONE);
	dc->slider_txt[SURF_ZTR] = make_text(dc->Wtrans_pop, "", 9,"ztr_txt", 
		    update_sliders, dc,wargs,n);
	XtAddCallback(dc->slider_txt[SURF_ZTR], XmNactivateCallback,
		    set_ztrans, dc);
	n = 0;
	SetPositionArgs(wargs, &n, 75, -1, 40, -1, XmATTACH_NONE);
	XtCreateManagedWidget("X:", xmLabelGadgetClass,
		    dc->Wtrans_pop, wargs, n);
	
	n = 0;
	SetPositionArgs(wargs, &n, 72, -1, 48, -1, XmATTACH_NONE);
	X_txt = make_text(dc->Wtrans_pop, "", 10,"xtr_txt", 
		    update_xytrans, dc,wargs,n);
	
	n = 0;
	SetPositionArgs(wargs, &n, 85, -1, 40, -1, XmATTACH_NONE);
	XtCreateManagedWidget("Y:", xmLabelGadgetClass,
		    dc->Wtrans_pop, wargs, n);
	
	n = 0;
	SetPositionArgs(wargs, &n, 82, -1, 48, -1, XmATTACH_NONE);
	Y_txt = make_text(dc->Wtrans_pop, "", 10,"ytr_txt", 
		    update_xytrans, dc,wargs,n);
	

    /* also need reset button */
	n = 0;
	SetPositionArgs(wargs, &n, -1, 95, 10, -1, XmATTACH_NONE);
	reset = XtCreateManagedWidget("Reset", xmPushButtonWidgetClass, 
		    dc->Wtrans_pop, wargs, n);
	XtAddCallback(reset,XmNactivateCallback,do_reset,dc);
	

	n = 0;
	SetPositionArgs(wargs, &n, -1, 95, -1, 95, XmATTACH_NONE);
	ok = XtCreateManagedWidget("Close", xmPushButtonWidgetClass, 
		    dc->Wtrans_pop, wargs, n);
	XtAddCallback(ok,XmNactivateCallback,unmanage_cb,dc->Wtrans_pop);
	XtAddCallback(ok,XmNactivateCallback,cursurf_update_attlabels,dc);
	
	first = 0;
	XtManageChild(dc->Wtrans_pop);
    }
    else
	XtManageChild(dc->Wtrans_pop);

    set_trans_widgets(dc);
}


