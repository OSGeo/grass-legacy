#include  "interface.h"

/* site_panel:
** the site panel creation routine 
*/

void 
sites_panel (Widget w, data_cell *dc, caddr_t call_data)
{
    int i,n;
    Arg wargs[30];
    static int first = 1;
    Widget wtmp, title, rad, wwrc;
    char **names;

    if(first) {

	inform(dc,"Calculating Positions");
	dc->panels[SITES].im_already_open = 1;
	dc->panels[SITES].name = XmStringCreateSimple("Site Panel");

	check_space(dc, SITES,0);

	n = 0;
	SetPositionArgs(wargs, &n, dc->here.toph, 100,
						-1, -1, XmATTACH_FORM);
	dc->panels[SITES].form = XtCreateManagedWidget("sform", 
		xmFormWidgetClass, dc->form_for_aux_control_panel, wargs,n);

	title = make_title(SITES, dc, 0);


/* make Option menu for selecting current site */

	n = 0;
	SetPositionArgs(wargs, &n, 10,-1,1,67, XmATTACH_NONE);
	dc->RCurSiteno = make_simple_options(dc->panels[SITES].form, 
		    "wcur_s", "Current:", s_labels(dc), 
		    GP_num_sites(), dc->CurSite, set_cur_site, wargs, n);

/* make new & delete buttons */

	n = 0;
	SetPositionArgs(wargs, &n, 10,25,70,82, NULL);
	wtmp = XtCreateManagedWidget("New", 
		  xmPushButtonWidgetClass, dc->panels[SITES].form, wargs, n);
	XtAddCallback(wtmp, XmNactivateCallback,new_site,dc);	

	n = 0;
	SetPositionArgs(wargs, &n, 10,25,83,99, NULL);
	wtmp = XtCreateManagedWidget("Delete", 
		  xmPushButtonWidgetClass, dc->panels[SITES].form, wargs, n);
	XtAddCallback(wtmp, XmNactivateCallback,delete_site,dc);	

/* make slider for size */

        n = 0;
        SetPositionArgs(wargs, &n, 25, -1, 4, -1, XmATTACH_NONE);
        XtSetArg (wargs[n], XmNorientation, XmHORIZONTAL); n++;
        wtmp = XtCreateManagedWidget("sitesiz_rc",
                xmRowColumnWidgetClass, dc->panels[SITES].form, wargs, n);


	n = 0;
        SetPositionArgs(wargs, &n, 25, -1, 4, -1, XmATTACH_NONE);
	XtSetArg(wargs[n],XmNscaleMultiple,10); n++;
        dc->sliders[SITE_SIZ] = make_slider(wtmp, 
		1000,0,100,2, "site size", update_sliders, update_sliders, 
		dc, 1, XmHORIZONTAL, wargs, n);
        XtAddCallback(dc->sliders[SITE_SIZ],XmNvalueChangedCallback,
                        site_set_displaymode,dc);

        n = 0;
	XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNtopPosition,25); n++;
	/*
        SetPositionArgs(wargs, &n, 25, -1, -1, -1, XmATTACH_NONE);
	*/
	XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(wargs[n], XmNleftWidget, dc->sliders[SITE_SIZ]); n++;
        dc->slider_txt[SITE_SIZ] = make_text(dc->panels[SITES].form, "",
                5, "Ssize", update_sliders, dc, wargs, n);
        XtAddCallback(dc->slider_txt[SITE_SIZ],XmNactivateCallback,
                        site_set_displaymode,dc);

/* make Arrows and text for selecting line width */

	n = 0;
	SetPositionArgs(wargs, &n, 40, -1, 4, -1, XmATTACH_NONE);
	make_arrows  (dc->panels[SITES].form, "Line Width",
		    1, 15, 2, &(dc->parrows[SWIDTH_ARWS]), wargs, &n);
	XtAddCallback(dc->parrows[SWIDTH_ARWS].up, 
		    XmNactivateCallback,site_set_displaymode,dc);	
	XtAddCallback(dc->parrows[SWIDTH_ARWS].down, 
		    XmNactivateCallback,site_set_displaymode,dc);	

/* make rc & toggles for determining object type */

	n = 0;
        SetPositionArgs(wargs, &n, 62,-1,4,-1, XmATTACH_NONE);
        XtSetArg(wargs[n],XmNspacing,0); n++;
        XtSetArg(wargs[n],XmNradioBehavior,TRUE); n++;
        wwrc = XtCreateManagedWidget("siteobj rc",
                xmRowColumnWidgetClass, dc->panels[SITES].form, wargs, n);

	n = 0;
	XtSetArg(wargs[n],XmNset,TRUE); n++;
	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	XtSetArg(wargs[n],XmNmarginHeight, 0); n++;
	dc->toggle_id[SITE_X] = XtCreateManagedWidget("use X", 
		    xmToggleButtonWidgetClass, wwrc, wargs, n);
	XtAddCallback(dc->toggle_id[SITE_X], XmNdisarmCallback,
			site_set_displaymode, dc);
	
	n = 0;
	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	XtSetArg(wargs[n],XmNmarginHeight, 0); n++;
	dc->toggle_id[SITE_SPHERE] = 
		    XtCreateManagedWidget("use sphere", 
		    xmToggleButtonWidgetClass, wwrc, wargs, n);
	XtAddCallback(dc->toggle_id[SITE_SPHERE], XmNdisarmCallback,
			site_set_displaymode, dc);
	
	n = 0;
	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	XtSetArg(wargs[n],XmNmarginHeight, 0); n++;
	dc->toggle_id[SITE_DIAMOND] = 
		    XtCreateManagedWidget("use diamond", 
		    xmToggleButtonWidgetClass, wwrc, wargs, n);
	XtAddCallback(dc->toggle_id[SITE_DIAMOND], XmNdisarmCallback,
			site_set_displaymode, dc);
	

/* make rc & toggles for determining placement (on surface or 3d) */

	n = 0;
        SetPositionArgs(wargs, &n, 35,-1,48,-1, XmATTACH_NONE);
        XtSetArg(wargs[n],XmNspacing,0); n++;
        XtSetArg(wargs[n],XmNradioBehavior,TRUE); n++;
        wwrc = XtCreateManagedWidget("site rc",
                xmRowColumnWidgetClass, dc->panels[SITES].form, wargs, n);

	n = 0;
/*      
maybe use some intelligence here...
	XtSetArg(wargs[n],XmNset,TRUE); n++;
*/
	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	XtSetArg(wargs[n],XmNmarginHeight, 0); n++;
	dc->toggle_id[SITE_3D] = XtCreateManagedWidget("3D Sites", 
		    xmToggleButtonWidgetClass, wwrc, wargs, n);
	XtAddCallback(dc->toggle_id[SITE_3D], XmNdisarmCallback,
			site_set_displaymode, dc);
	
	n = 0;
	XtSetArg(wargs[n],XmNset,TRUE); n++;
	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	XtSetArg(wargs[n],XmNmarginHeight, 0); n++;
	dc->toggle_id[SITE_ONSURF] = 
		    XtCreateManagedWidget("Display on surface(s):", 
		    xmToggleButtonWidgetClass, wwrc, wargs, n);
	XtAddCallback(dc->toggle_id[SITE_ONSURF], XmNdisarmCallback,
			site_set_displaymode, dc);
	
/* make PushButton for selecting color */

	n = 0;
	SetPositionArgs(wargs, &n, 85, 95, 45, 65, NULL);
	dc->toggle_id[S_COLOR] = XtCreateManagedWidget("Color", 
		xmPushButtonWidgetClass, 
		dc->panels[SITES].form, wargs, n);	

	XtAddCallback(dc->toggle_id[S_COLOR],XmNactivateCallback,pop_color,dc);	

	set_button_colors(dc->toggle_id[S_COLOR], dc, SITES_CELL);
	  

/* 
	n = 0;
	SetPositionArgs(wargs, &n, 85, -1, 10, -1, XmATTACH_NONE);
	wtmp = XtCreateManagedWidget("Qdraw", xmPushButtonWidgetClass, 
		dc->panels[SITES].form, wargs, n);	

	XtAddCallback(wtmp,XmNactivateCallback,do_fastsitedraw,dc);

Button to test site decimation - will become positioning panel for sites */


/* make ScrolledWindow with RowCol containing check boxes to mark
   which surfaces current site is to be displayed on */


	n = 0;
	SetPositionArgs(wargs, &n, 50, 80, 45, 95, NULL);
	XtSetArg(wargs[n], XmNscrollingPolicy,XmAUTOMATIC); n++;
	dc->Sscroll = XmCreateScrolledWindow(dc->panels[SITES].form, 
		 "on_surfs", wargs, n);	

	n = 0;
	wwrc = XtCreateManagedWidget("Ssurf_buttons", xmRowColumnWidgetClass,
		    dc->Sscroll, wargs, n);
	
	names = sf_labels(dc);
	for(i=0; i < GS_num_surfs(); i++){
	    n = 0;

	    if(GP_surf_is_selected(dc->hSite[dc->CurSite], dc->hSurf[i])){
		XtSetArg(wargs[n],XmNset,TRUE); n++;
	    }

	    XtCreateManagedWidget(names[i], xmToggleButtonGadgetClass,
		    wwrc, wargs, n);
	}

	n = 0;
	XtSetArg(wargs[n], XmNworkWindow,wwrc); n++;
	XtSetValues(dc->Sscroll, wargs, n);

	XtManageChild(dc->Sscroll);
	    
	closeb(SITES,dc,0);

	site_update_displaymode(NULL, dc, NULL);

	inform(dc,"Done");

	first = 0;
    }


/*---------------------------------------------------------------------
this section checks to see if the panel is already open and acts    --
accordingly                                                        --
---------------------------------------------------------------------*/

    else if(dc->panels[SITES].im_already_open){
	dc->constant = SITES;
	pops(w, dc, SITES);
	return;
    }

    else {
        inform(dc,"Calculating Positions");	
	_update_site_options(dc);
	_update_Sscroll_options(dc);  /* combine these two? */
        reshow(dc,SITES, 1); 
	site_update_displaymode(NULL, dc, NULL);
        inform(dc,"Done");	
    }

}


