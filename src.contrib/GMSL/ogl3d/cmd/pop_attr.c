/* pop_attr:
** controls pop up dialog for changing attributes for surface 
** serves as callback routine for any attribute selection buttons
*/


#include "interface.h"	

static Widget Newconst;

static void 
get_topofile (Widget w, data_cell *dc, caddr_t ignored)
{
#ifdef DEBUG
fprintf(stderr, "get_topofile called\n");
#endif

    if(XtIsManaged(Newconst))
	XtUnmanageChild(Newconst);
    pops(w, dc, ELEVATION_FILE);
}

static void 
get_colorfile (Widget w, data_cell *dc, caddr_t ignored)
{
#ifdef DEBUG
fprintf(stderr, "get_colorfile called\n");
#endif

    if(XtIsManaged(Newconst))
	XtUnmanageChild(Newconst);
    pops(w, dc, COLOR_FILE);
}

static void 
get_rasterfile (Widget w, data_cell *dc, caddr_t ignored)
{
#ifdef DEBUG
fprintf(stderr, "get_rasterfile called\n");
#endif

    if(XtIsManaged(Newconst))
	XtUnmanageChild(Newconst);
    pops(w, dc, RASTER_FILE);
}

static void 
remove_mask (Widget w, data_cell *dc, caddr_t ignored)
{
int n;
Arg wargs[15];
Widget wid;
    
    dc->Cur_Atts[dc->CurAtt].use_map = 0;
    dc->Cur_Atts[dc->CurAtt].constant2 = C2_NOTSET;

}

static void 
show_newconst (Widget w, data_cell *dc, caddr_t ignored)
{
int n;
Arg wargs[15];
Widget wid;
    
    dc->Cur_Atts[dc->CurAtt].use_map = 0;
    if(C2_NOTSET == dc->Cur_Atts[dc->CurAtt].constant2) 
	dc->Cur_Atts[dc->CurAtt].constant2 = 0;
    if(!XtIsManaged(Newconst))
	XtManageChild(Newconst);

}


void 
pop_attr (Widget w, data_cell *dc, caddr_t client_data)
{
unsigned int *c = NULL;
Widget newmap, newconst, accept, cancel, slider_rc;
Widget close;
int n;
Arg wargs[15];
char str[80];
void (*cb1)(), (*cb2)();


    cb2 = toggle;   /* test */
    if(w == dc->Watt[ATT_TOPO]){
	dc->CurAtt = ATT_TOPO;
	cb1 = get_topofile;
	cb2 = show_newconst; 
    }
    else if (w == dc->Watt[ATT_COLOR]){
	dc->CurAtt = ATT_COLOR;
	cb1 = get_colorfile;
	cb2 = show_newconst;
    }
    else if (w == dc->Watt[ATT_MASK]){
	dc->CurAtt = ATT_MASK;
	cb1 = get_rasterfile;
	cb2 = remove_mask; 
    }
    else if (w == dc->Watt[ATT_SHINE]){
	dc->CurAtt = ATT_SHINE;
	cb1 = get_rasterfile;
	cb2 = show_newconst; 
    }
    else if (w == dc->Watt[ATT_TRANSP]){
	dc->CurAtt = ATT_TRANSP;
	cb1 = get_rasterfile;
	cb2 = show_newconst; 
    }
    else if (w == dc->Watt[ATT_EMIT]){
	dc->CurAtt = ATT_EMIT;
	cb1 = get_rasterfile;
	cb2 = show_newconst; 
    }

    n = 0;
    XtSetArg(wargs[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;
    XtSetArg(wargs[n],XmNautoUnmanage, FALSE); n++;
    XtSetArg(wargs[n],XmNdialogTitle, 
		    XmStringCreateSimple("Change Attribute")); n++;

/*  affects constraints when destroyed  */
/*
    dc->Wattr_pop = XmCreateFormDialog(dc->form_for_aux_control_panel,
		    "Change Attribute", wargs, n);
*/
		    
/*  works good, but not centered over control panel.(centered over button) */
    dc->Wattr_pop = XmCreateFormDialog(w,
		    "Change Attribute", wargs, n);
    
    strcpy(str, "Attribute:    ");
    strcat(str, dc->Cur_Atts[dc->CurAtt].name);
    n = 0;
    SetPositionArgs(wargs, &n, 5, 15, -1, -1, XmATTACH_FORM);
    XtSetArg(wargs[n], XmNlabelString, 
		    XmStringCreateSimple(str)); n++;
    XtCreateManagedWidget(str, xmLabelGadgetClass, dc->Wattr_pop, wargs, n);
    
    strcpy(str, "Status: ");
    n = 0;
    SetPositionArgs(wargs, &n, 35, 45, -1, -1, XmATTACH_FORM);
    XtSetArg(wargs[n], XmNlabelString, 
		    XmStringCreateSimple(str)); n++;
    XtCreateManagedWidget(str, xmLabelGadgetClass, dc->Wattr_pop, wargs, n);

    strcpy(str, dc->Cur_Atts[dc->CurAtt].status);
    n = 0;
    SetPositionArgs(wargs, &n, 50, 55, -1, -1, XmATTACH_FORM);
    XtSetArg(wargs[n], XmNlabelString, XmStringCreateSimple(str)); n++;
    dc->att_status = XtCreateManagedWidget(str, 
			xmLabelGadgetClass, dc->Wattr_pop, wargs, n);

    if(ATT_COLOR == dc->CurAtt){
	n = 0;
	SetPositionArgs(wargs, &n, 60, 80, 20, 80, XmATTACH_FORM);
	XtSetArg(wargs[n], XmNbackground,dc->cells[SURF_CELL]); n++;
	if(!(dc->Cur_Atts[dc->CurAtt].use_map))
	    Newconst = XtCreateManagedWidget("", xmLabelWidgetClass, 
			dc->Wattr_pop, wargs, n);
	else
	    Newconst = XtCreateWidget("", xmLabelWidgetClass, 
			dc->Wattr_pop, wargs, n);
    }
    else if(ATT_TOPO == dc->CurAtt){
	sprintf(str, "%f", dc->Cur_Atts[dc->CurAtt].constant);
	n = 0;
	SetPositionArgs(wargs, &n, 60, -1, 20, -1, XmATTACH_NONE);
	XtSetArg (wargs[n], XmNvalue, str); n++;
	Newconst = XmCreateText(dc->Wattr_pop, "att_const", wargs, n);
	XtAddCallback(Newconst, XmNactivateCallback, curatt_set_const, dc);
	XtAddCallback(Newconst, XmNactivateCallback, curatt_update_status, dc);
    }

    else{
	n=0;
	SetPositionArgs(wargs, &n, 60, -1, 20, 80, XmATTACH_NONE);
	Newconst = XtCreateWidget("slider_rc", xmRowColumnWidgetClass,
		    dc->Wattr_pop, wargs, n); 
	
	n=0;
	dc->sliders[ATTR_CON] = make_slider (Newconst, 255, 0, 
		     (int)dc->Cur_Atts[dc->CurAtt].constant,
		     0, "const_slider", update_sliders, update_sliders,
		     dc, 0, XmHORIZONTAL, wargs, n);
	XtAddCallback(dc->sliders[ATTR_CON], XmNvalueChangedCallback, 
			    curatt_set_const, dc);
	XtAddCallback(dc->sliders[ATTR_CON], XmNvalueChangedCallback, 
			    curatt_update_status, dc);

	sprintf(str, "%d", (int)dc->Cur_Atts[dc->CurAtt].constant);
	n = 0;
	dc->slider_txt[ATTR_CON] = make_text(Newconst, str,
			5, "const_txt", update_sliders, dc, wargs, n);
	XtAddCallback(dc->slider_txt[ATTR_CON], XmNactivateCallback, 
			    curatt_set_const, dc);
	XtAddCallback(dc->slider_txt[ATTR_CON], XmNactivateCallback, 
			    curatt_update_status, dc);
    }

    n = 0;
    SetPositionArgs(wargs, &n, 20, 30, 10, 45, XmATTACH_NONE);
    newmap = XtCreateManagedWidget("New Map", xmPushButtonWidgetClass, 
		dc->Wattr_pop, wargs, n);
    XtAddCallback(newmap,XmNactivateCallback,cb1,dc);

    if(ATT_MASK == dc->CurAtt)
	sprintf(str, "Remove Mask");
    else
	sprintf(str, "New Constant");
    n = 0;
    SetPositionArgs(wargs, &n, 20, 30, 55, 90, XmATTACH_NONE);
    newconst = XtCreateManagedWidget(str, 
		xmPushButtonWidgetClass, dc->Wattr_pop, wargs, n);
    XtAddCallback(newconst,XmNactivateCallback,cb2,dc);
    if(ATT_COLOR == dc->CurAtt)
	XtAddCallback(newconst,XmNactivateCallback,pop_color,dc);
    
    if(ATT_MASK == dc->CurAtt){
	XtAddCallback(newconst, XmNactivateCallback, 
			    curatt_update_status, dc);
	n = 0;
	SetPositionArgs(wargs, &n, 70, -1, 15, -1, XmATTACH_NONE);
	XtSetArg(wargs[n],XmNindicatorSize,15); n++;
	XtSetArg(wargs[n],XmNspacing,0); n++;
	if(C2_INVMASK == dc->Cur_Atts[dc->CurAtt].constant2){
	    XtSetArg(wargs[n],XmNset,True); n++;
	}
	dc->toggle_id[INV_MASK] = XtCreateManagedWidget("Invert Mask", 
		    xmToggleButtonWidgetClass, dc->Wattr_pop, wargs, n);
	XtAddCallback(dc->toggle_id[INV_MASK],XmNvalueChangedCallback,
		    set_const2,dc);
    }

    n = 0;
    SetPositionArgs(wargs, &n, -1, 95, 5, -1, XmATTACH_NONE);
    accept = XtCreateManagedWidget("Accept", xmPushButtonWidgetClass, 
		dc->Wattr_pop, wargs, n);
    XtAddCallback(accept,XmNactivateCallback,change_att,dc);
    XtAddCallback(accept,XmNactivateCallback,destroy_cb,dc->Wattr_pop);

    n = 0;
    SetPositionArgs(wargs, &n, -1, 95, -1, 95, XmATTACH_NONE);
    cancel = XtCreateManagedWidget("Cancel", xmPushButtonWidgetClass, 
		dc->Wattr_pop, wargs, n);
    XtAddCallback(cancel,XmNactivateCallback, curatt_reset,dc);
    XtAddCallback(cancel,XmNactivateCallback,destroy_cb,dc->Wattr_pop);


    switch(dc->CurAtt){
	case ATT_SHINE:
	case ATT_TRANSP:
	case ATT_EMIT:
	    break;
	case ATT_TOPO:
	    break;
	case ATT_COLOR:
	    break;
	case ATT_MASK:
	    break;
    }



#ifdef OLD	
    n = 0;
    SetPositionArgs(wargs, &n, 5, 50, 5, 55, NULL);
    slider_rc = XtCreateManagedWidget("val slider", xmRowColumnWidgetClass,
		dc->Wattr_pop, wargs, n); 
#endif	

    if(!XtIsManaged(dc->Wattr_pop))
	XtManageChild(dc->Wattr_pop);
}



