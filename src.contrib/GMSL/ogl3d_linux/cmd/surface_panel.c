
/* surface_panel:
** creates a panel that is used for setting surface options
*/


#include "interface.h"

void 
surface_panel (Widget w, data_cell *dc, caddr_t call_data)
{
    int i, n, t, b;
    Arg wargs[30];
    static int first = 1;
    Widget wtmp, wtmp2, title, sep, rc, rc2, rc3;

    if(first){

	inform(dc,"Calculating Positions");

	cursurf_set_curatts(dc);

	dc->panels[SURFACE].im_already_open = 1;
	dc->panels[SURFACE].name = XmStringCreateSimple("Surface Panel");

        check_space(dc, SURFACE, 1);

	n = 0;
	SetPositionArgs(wargs, &n, dc->here.toph,dc->here.botth,
			-1,-1, XmATTACH_FORM);
	dc->panels[SURFACE].form = XtCreateManagedWidget("sform", 
		xmFormWidgetClass, dc->form_for_aux_control_panel, wargs,n);

	title = make_title(SURFACE, dc, 0);

/*---------------------------------------------------------
make the arrow buttons for the grid resolution control --
---------------------------------------------------------*/

	n = 0;
	XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(wargs[n], XmNtopWidget, title); n++;
	XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(wargs[n], XmNbottomPosition, 17); n++;
	XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(wargs[n], XmNrightPosition, 50); n++;
	make_arrows  (dc->panels[SURFACE].form, "Grid Resolution",
		    1, 99, 2, &(dc->parrows[G_RES_ARWS]), wargs, &n);
	XtAddCallback(dc->parrows[G_RES_ARWS].up, 
		    XmNactivateCallback,surf_set_displaymode,dc);	
	XtAddCallback(dc->parrows[G_RES_ARWS].down, 
		    XmNactivateCallback,surf_set_displaymode,dc);	

/*------------------------------------------------------------
make the arrow buttons for the polygon resolution control -- 
------------------------------------------------------------*/

	n = 0;
	SetPositionArgs(wargs, &n, 19,32,-1,55, XmATTACH_FORM);
	make_arrows  (dc->panels[SURFACE].form, "Polygon Resolution",
		    1, 99, 2, &(dc->parrows[P_RES_ARWS]), wargs, &n);
	XtAddCallback(dc->parrows[P_RES_ARWS].up, 
		    XmNactivateCallback,surf_set_displaymode,dc);	
	XtAddCallback(dc->parrows[P_RES_ARWS].down, 
		    XmNactivateCallback,surf_set_displaymode,dc);	
	

/*-------------------------------------------------------------------
make the toggle buttons that determine the display method        --
for the data.                                                    --
-------------------------------------------------------------------*/
	n = 0;
	SetPositionArgs(wargs, &n, 30,35,60,90, XmATTACH_FORM);
	XtSetArg(wargs[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
	XtCreateManagedWidget("Shading:",
		xmLabelWidgetClass, dc->panels[SURFACE].form, wargs,n);

	n = 0;
	SetPositionArgs(wargs, &n, 35,50,60,-1, XmATTACH_FORM);
	XtSetArg(wargs[n],XmNspacing,0); n++;
	XtSetArg(wargs[n],XmNradioBehavior,TRUE); n++;
	rc2 = XtCreateManagedWidget("Surface rc", 
		xmRowColumnWidgetClass, dc->panels[SURFACE].form, wargs, n);

	n = 0;
	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	XtSetArg(wargs[n],XmNspacing,0); n++;
	dc->toggle_id[FLAT_RND] = XtCreateManagedWidget("Flat", 
		xmToggleButtonWidgetClass, rc2, wargs,n);
	XtAddCallback(dc->toggle_id[FLAT_RND], XmNdisarmCallback, 
		surf_set_displaymode, dc);	
	
	XtSetArg(wargs[n],XmNset,TRUE); n++;
	dc->toggle_id[GOURAUD_RND] = XtCreateManagedWidget("Gouraud", 
		xmToggleButtonWidgetClass, rc2, wargs,n);
	XtAddCallback(dc->toggle_id[GOURAUD_RND], XmNdisarmCallback, 
		surf_set_displaymode, dc);	


/*-------------------------------------------------------------------
make the toggle buttons that determine the drawing method        --
for the data.                                                    --
-------------------------------------------------------------------*/
	n = 0;
	SetPositionArgs(wargs, &n,  6,30,60,99, NULL);
	XtSetArg(wargs[n],XmNradioBehavior,TRUE); n++;
	rc = XtCreateManagedWidget("Rendering rc", 
		xmRowColumnWidgetClass, dc->panels[SURFACE].form, wargs, n);

	n = 0;
	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	XtSetArg(wargs[n],XmNspacing,0); n++;
	dc->toggle_id[WIRE_RND] = XtCreateManagedWidget("Wire", 
		xmToggleButtonWidgetClass, rc, wargs,n);
	XtAddCallback(dc->toggle_id[WIRE_RND], XmNdisarmCallback, 
		surf_set_displaymode, dc);	

	XtSetArg(wargs[n],XmNset,TRUE); n++;
	dc->toggle_id[POLY_RND] = XtCreateManagedWidget("Polygon", 
		xmToggleButtonWidgetClass, rc, wargs,n);
	XtAddCallback(dc->toggle_id[POLY_RND], XmNdisarmCallback, 
		surf_set_displaymode, dc);	

	n--;   /*  XmNset, False  */
	dc->toggle_id[WIRE_POLY_RND] = XtCreateManagedWidget("Wire/Polygon", 
		xmToggleButtonWidgetClass, rc, wargs,n);
	XtAddCallback(dc->toggle_id[WIRE_POLY_RND], XmNdisarmCallback, 
		surf_set_displaymode, dc);	

/*-------------------------------------------------------------------
make the toggle buttons that determine scope of changes made     --
on this panel.                                                   --
-------------------------------------------------------------------*/
	n = 0;
	SetPositionArgs(wargs, &n, 32,50,1,52, NULL);
	XtSetArg(wargs[n],XmNradioBehavior,TRUE); n++;
	rc3 = XtCreateManagedWidget("Scope rc", 
		xmRowColumnWidgetClass, dc->panels[SURFACE].form, wargs, n);

	n = 0;
	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	XtSetArg(wargs[n],XmNspacing,0); n++;
	XtSetArg(wargs[n],XmNset,FALSE); n++;
	dc->toggle_id[SCOPE_CSO]=XtCreateManagedWidget("Current Surface Only", 
		xmToggleButtonWidgetClass, rc3, wargs,n);
	XtAddCallback(dc->toggle_id[SCOPE_CSO], XmNvalueChangedCallback, 
		surf_set_displaymode, dc);	

	XtSetArg(wargs[n],XmNset,TRUE); n++;
	dc->toggle_id[SCOPE_AS] = XtCreateManagedWidget("All Surfaces", 
		xmToggleButtonWidgetClass, rc3, wargs,n);
	XtAddCallback(dc->toggle_id[SCOPE_AS], XmNvalueChangedCallback, 
		surf_set_displaymode, dc);	

/*------------------------------------------------------------------------*/
/*------------------------------------------------------------------------*/
/*------------------------------------------------------------------------*/
/* seperator for current surface status */
	
	n = 0;
	SetPositionArgs(wargs, &n, 49,50,1,99, XmATTACH_FORM);
	sep = XtCreateManagedWidget("sep1",xmSeparatorWidgetClass,
		    dc->panels[SURFACE].form, wargs,n);


/* make Option menu for selecting current surface */

	n = 0;
	SetPositionArgs(wargs, &n, 51,-1,-1,-1, XmATTACH_NONE);
	dc->RCurSurfno = make_simple_options(dc->panels[SURFACE].form, 
		    "wcur_sf", "Current:", sf_labels(dc), 
		    GS_num_surfs(), dc->CurSurf, set_cur_surf, wargs, n);

/* make new & delete buttons */

	n = 0;
	SetPositionArgs(wargs, &n, 51,60,70,83, NULL);
	wtmp = XtCreateManagedWidget("New", 
		  xmPushButtonWidgetClass, dc->panels[SURFACE].form, wargs, n);
	XtAddCallback(wtmp, XmNactivateCallback,new_surf,dc);	

	n = 0;
	SetPositionArgs(wargs, &n, 51,60,83,99, NULL);
	wtmp = XtCreateManagedWidget("Delete", 
		  xmPushButtonWidgetClass, dc->panels[SURFACE].form, wargs, n);
	XtAddCallback(wtmp, XmNactivateCallback,delete_surf,dc);	

	n = 0;
	SetPositionArgs(wargs, &n, 60, -1, 3, -1, XmATTACH_NONE);
	XtSetArg(wargs[n], XmNlabelString, 
		    XmStringCreateSimple("Draw Current")); n++;
	wtmp= XtCreateManagedWidget("draw_cursurf", xmPushButtonWidgetClass, 
		    dc->panels[SURFACE].form, wargs,n);
	XtAddCallback(wtmp, XmNactivateCallback, draw_cursurf, dc);


/* make (attribute/change | status ) for: 
    topology
    color
    mask
    shineyness
    transparency
    emission
*/

	for(i = 1 ; i < MAX_ATTS; i++){

	    t = 62+i*5;
	    b = 62+(i+1)*5;

	    n = 0;
	    SetPositionArgs(wargs, &n, t, b, 2, 28, NULL);
	    dc->Watt[i] = XtCreateManagedWidget(dc->Cur_Atts[i].name, 
			xmPushButtonWidgetClass, 
			dc->panels[SURFACE].form, wargs, n);

/*
	    if (i == ATT_SHINE || i == ATT_EMIT) 
		XtAddCallback(dc->Watt[i], XmNactivateCallback,nyi_pops,dc);
*/

	    if (i == ATT_TRANSP){
		if(GS_has_transparency())
		    XtAddCallback(dc->Watt[i], XmNactivateCallback,pop_attr,dc);
		else
		    XtAddCallback(dc->Watt[i], XmNactivateCallback,
			    bf_unavail,dc);
	    }
	    else
		XtAddCallback(dc->Watt[i], XmNactivateCallback,pop_attr,dc);	

	    n = 0;
	    SetPositionArgs(wargs, &n, t, b, 30, 74, NULL);
	    XtSetArg(wargs[n],XmNalignment, XmALIGNMENT_BEGINNING); n++;
	    dc->att_label[i] = XtCreateManagedWidget(dc->Cur_Atts[i].status,
		    xmLabelWidgetClass, dc->panels[SURFACE].form, wargs,n);

	    if(i == ATT_TOPO){
		n = 0;
		SetPositionArgs(wargs, &n, t, b, 75, 99, NULL);
		XtSetArg(wargs[n],XmNindicatorSize,12); n++;
		XtSetArg(wargs[n],XmNspacing,1); n++;
		dc->toggle_id[NZ_TOPO] = XtCreateManagedWidget("No Zeros", 
			xmToggleButtonWidgetClass, 
			dc->panels[SURFACE].form, wargs,n);
		XtAddCallback(dc->toggle_id[NZ_TOPO], XmNvalueChangedCallback,
			set_const2,dc);	
	    }
	    if(i == ATT_COLOR){
		n = 0;
		SetPositionArgs(wargs, &n, t, b, 75, 99, NULL);
		XtSetArg(wargs[n],XmNindicatorSize,12); n++;
		XtSetArg(wargs[n],XmNspacing,1); n++;
		dc->toggle_id[NZ_COLOR] = XtCreateManagedWidget("No Zeros", 
			xmToggleButtonWidgetClass, 
			dc->panels[SURFACE].form, wargs,n);
		XtAddCallback(dc->toggle_id[NZ_COLOR], XmNvalueChangedCallback,
			set_const2,dc);	
	    }
	    if(i == ATT_MASK){
		n = 0;
		SetPositionArgs(wargs, &n, t, b, 75, 99, NULL);
		XtSetArg(wargs[n], XmNbackground,dc->cells[GRID_CELL]); n++;
		dc->Swirecolor = XtCreateManagedWidget("Wire Color", 
			xmPushButtonWidgetClass, 
			dc->panels[SURFACE].form, wargs,n);
		XtAddCallback(dc->Swirecolor,XmNactivateCallback,pop_color,dc);
	    }
	    if(i == ATT_TRANSP){
		n = 0;
		SetPositionArgs(wargs, &n, t, b, 75, 99, NULL);
		dc->Stranslate = XtCreateManagedWidget("Position", 
			xmPushButtonWidgetClass, 
			dc->panels[SURFACE].form, wargs,n);
		XtAddCallback(dc->Stranslate,XmNactivateCallback,pop_transl,dc);
	    }


	}


	closeb(SURFACE,dc,0);

	inform(dc,"Done");

	first = 0;

	/* SET LABELS FOR SURFACE and ATT STATUS */
	cursurf_update_attlabels(dc->panels[SURFACE].form, dc, NULL);
	/* SET TOGGLES AND ARROWS for DISPLAY MODE */
	surf_update_displaymode(dc->panels[SURFACE].form, dc, NULL);

    }


    else if(dc->panels[SURFACE].im_already_open){
	dc->constant = SURFACE;
	pops(w, dc, SURFACE);
	return;
    }

    else {
	inform(dc,"Calculating Positions");

	_update_surface_options(dc);

	/* SET LABELS FOR SURFACE and ATT STATUS */
	cursurf_update_attlabels(dc->panels[SURFACE].form, dc, NULL);
	/* SET TOGGLES AND ARROWS for DISPLAY MODE */
	surf_update_displaymode(dc->panels[SURFACE].form, dc, NULL);

	reshow(dc,SURFACE, 1);
	inform(dc,"Done");
    }

}


