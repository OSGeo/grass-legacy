

/* pop_color:
** controls pop up dialog for color selection
** serves as callback routine for any color selection buttons
*/


#include "interface.h"	
#include "coldefs.h" 

static XColor palcol[CONST_COLS];

static char col_prompt[MAX_DYN_COLORS][40];


void 
pop_color (Widget w, data_cell *dc, caddr_t client_data)
{

/* Changed to unsigned short from unsigned long for 16-bit color*/

unsigned short *c = NULL;
Widget button_rc, slider_rc;
Widget close, buttons[CONST_COLS];
int bsize,n, r,g,b,i,col_cell, modal=0;
Arg wargs[15];
char *str;
static int first = 1;

    if(first){
	set_color_prompts();
	set_palette_colors(dc->dpy, dc->cmap);

    }

    if(w == dc->toggle_id[V_COLOR]){
	col_cell = VECT_CELL;
	modal=1; /* force MODAL operation */ 
	if(!first) XtDestroyWidget(dc->Wcolor_pop);
    }
    else if(w == dc->toggle_id[S_COLOR]){
	col_cell = SITES_CELL;
	modal=1; /* force MODAL operation */ 
	if(!first) XtDestroyWidget(dc->Wcolor_pop);
    }
    else if(w == dc->toggle_id[BG_COLOR]){
	col_cell = BG_CELL;
    }
    else if(w == dc->Swirecolor){
	col_cell = GRID_CELL;
	modal=1; /* force MODAL operation */ 
	if(!first) XtDestroyWidget(dc->Wcolor_pop);
    }
    else{
	col_cell = SURF_CELL;
	modal=1; /* force MODAL operation */ 
	if(!first) XtDestroyWidget(dc->Wcolor_pop);
    }
    str = col_prompt[col_cell];
    c = &(dc->cells[col_cell]);

    if(!c) return;


    dc->colors.pixel = *c;
    dc->colors.flags = DoRed | DoGreen | DoBlue;

    if(first || modal){

	XgDoHourGlass(XtParent(w));

	n = 0;
	if(modal){
	    XtSetArg(wargs[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL);
	    n++;
	}
	dc->Wcolor_pop = XmCreateFormDialog(w, "Select Color", wargs, n);
	
	n = 0;
	SetPositionArgs(wargs, &n, 5, 45, 5, 55, NULL);
	slider_rc = XtCreateManagedWidget("sliders", xmRowColumnWidgetClass,
		    dc->Wcolor_pop, wargs, n); 
	
	n=0;
	XtSetArg(wargs[n],XmNbackground, dc->cells[RED_CELL]); n++;
	dc->sliders[COL_RED] = make_slider (slider_rc, 65535, 0, 
		     (int)dc->colors.red,
		     0, "", update_sliders, update_sliders,
		     dc, 0, XmHORIZONTAL, wargs, n);

	n = 0;
	dc->slider_txt[COL_RED] = make_text(slider_rc, "",
			5, "redtxt", update_sliders, dc, wargs, n);

	n=0;
	XtSetArg(wargs[n],XmNbackground, dc->cells[GRN_CELL]); n++;
	dc->sliders[COL_GRN] = make_slider (slider_rc, 65535, 0, 
		     (int)dc->colors.green,
		     0, "", update_sliders, update_sliders,
		     dc, 0, XmHORIZONTAL, wargs, n);
	
	n = 0;
	dc->slider_txt[COL_GRN] = make_text(slider_rc, "",
			5, "grntxt", update_sliders, dc, wargs, n);

	n=0;
	XtSetArg(wargs[n],XmNbackground, dc->cells[BLU_CELL]); n++;
	dc->sliders[COL_BLU] = make_slider (slider_rc, 65535, 0, 
		     (int)dc->colors.blue,
		     0, "", update_sliders, update_sliders,
		     dc, 0, XmHORIZONTAL, wargs, n);
	
	n = 0;
	dc->slider_txt[COL_BLU] = make_text(slider_rc, "",
			5, "blutxt", update_sliders, dc, wargs, n);

	n = 0;
	SetPositionArgs(wargs, &n, 5, 45, 60, 92, NULL);
	XtSetArg(wargs[n],XmNbackground, dc->colors.pixel); n++;
	dc->showcolor = XtCreateManagedWidget("", xmLabelWidgetClass,
		    dc->Wcolor_pop, wargs, n);

	n = 0;
	SetPositionArgs(wargs, &n, 50, 80, 5, 95, NULL);
	XtSetArg(wargs[n],XmNspacing, 0); n++;
	XtSetArg(wargs[n],XmNnumColumns, COLUMNS ); n++;
	XtSetArg(wargs[n],XmNpacking, XmPACK_COLUMN); n++;
	XtSetArg(wargs[n],XmNadjustLast, FALSE); n++;
	button_rc = XtCreateManagedWidget("buttons", xmRowColumnWidgetClass,
		    dc->Wcolor_pop, wargs, n); 


       /* make constant color cells - make labels instead of Pbuttons? */
	n=0;
	for(i = 0; i< CONST_COLS; i++){
	    XtSetArg(wargs[n],XmNbackground, palcol[i].pixel); n++;
	    buttons[i] = XtCreateManagedWidget("  ", 
			xmPushButtonWidgetClass, button_rc, wargs, n);    
	    XtAddCallback(buttons[i],XmNactivateCallback,color_pushed,dc);
	    if(col_cell == GRID_CELL) 
		XtAddCallback(buttons[i],XmNactivateCallback,
				unset_gridcolor,dc);
	    else if(col_cell == SITES_CELL) 
		XtAddCallback(buttons[i],XmNactivateCallback,
				unset_site_colorfile,dc);
	    n--;
	}

	if(col_cell == GRID_CELL){ 
	    n = 0;
	    SetPositionArgs(wargs, &n, 80, -1, 10, 90, XmATTACH_NONE);
	    XtSetArg(wargs[n],XmNindicatorSize,15); n++;
	    XtSetArg(wargs[n],XmNspacing,0); n++;
	    if(WC_COLOR_ATT == dc->Surf_Settings[dc->CurSurf].wire_color)
		XtSetArg(wargs[n],XmNset,True); n++;
	    dc->toggle_id[COL_WIRE_RND] = 
		    XtCreateManagedWidget("Use Surface Color", 
		    xmToggleButtonWidgetClass, dc->Wcolor_pop, wargs,n);
	    XtAddCallback(dc->toggle_id[COL_WIRE_RND], XmNvalueChangedCallback, 
		    cursurf_gridcolor_surface, dc);	
	}
	else if(col_cell == SITES_CELL){ 
	    n = 0;
	    SetPositionArgs(wargs, &n, 80, -1, 10, 90, XmATTACH_NONE);
	    XtSetArg(wargs[n],XmNindicatorSize,15); n++;
	    XtSetArg(wargs[n],XmNspacing,0); n++;
	    if(ST_ATT_COLOR & dc->Site_Settings[dc->CurSite].attrmode)
		XtSetArg(wargs[n],XmNset,True); n++;
	    dc->toggle_id[COL_SITEFILE] = 
		    XtCreateManagedWidget("Use Color Table", 
		    xmToggleButtonWidgetClass, dc->Wcolor_pop, wargs,n);
	    XtAddCallback(dc->toggle_id[COL_SITEFILE], XmNvalueChangedCallback, 
		    cursite_set_colorfile, dc);	
	}

	/* make close button */
	n = 0;
	SetPositionArgs(wargs, &n, 90, -1, 75, -1, XmATTACH_NONE);
	if(modal){
	    close = XtCreateManagedWidget("  OK  ", 
			xmPushButtonWidgetClass, dc->Wcolor_pop, wargs, n);
	    if(col_cell == SURF_CELL){
		XtAddCallback(close,XmNactivateCallback,curatt_change_rgb,dc);
		XtAddCallback(close,XmNactivateCallback,
				curatt_update_status,dc);
	    }
	    if(col_cell == GRID_CELL){ 
		XtAddCallback(close,XmNactivateCallback,
				cursurf_change_gridcolor,dc);
	    }
	    if(col_cell == VECT_CELL){ 
		XtAddCallback(close,XmNactivateCallback,
				curvect_change_rgb,dc);
	    }
	    if(col_cell == SITES_CELL){ 
		XtAddCallback(close,XmNactivateCallback,
				cursite_change_rgb,dc);
	    }

	    XtAddCallback(close,XmNactivateCallback,destroy_cb,dc->Wcolor_pop);
	}
	else{
	    close = XtCreateManagedWidget("Close", 
			xmPushButtonWidgetClass, dc->Wcolor_pop, wargs, n);
	    XtAddCallback(close,XmNactivateCallback,unmanage_cb,dc->Wcolor_pop);
	}

	/* make label button */
	n = 0;
	SetPositionArgs(wargs, &n, 90, -1, 10, -1, XmATTACH_NONE);
	XtSetArg(wargs[n], XmNlabelString, XmStringCreateSimple(str)); n++;
	dc->color_pop_lab = XtCreateManagedWidget(str,
			xmLabelGadgetClass, dc->Wcolor_pop, wargs, n);
	first=modal;

    }

    else{
	
	n = 0;
	XtSetArg(wargs[n],XmNbackground, dc->colors.pixel); n++;
	XtSetValues(dc->showcolor, wargs, n);

	n = 0;
	XtSetArg(wargs[n],XmNlabelString, XmStringCreateSimple(str)); n++;
	XtSetValues(dc->color_pop_lab, wargs, n);
	
    }
fprintf(stderr,"pixe %d red %d green %d blue %d\n",dc->colors.pixel,dc->colors.red,dc->colors.green,dc->colors.blue);
    XQueryColor(dc->dpy, dc->cmap, &(dc->colors));
    update_rgb(dc);

    if(!XtIsManaged(dc->Wcolor_pop))
	XtManageChild(dc->Wcolor_pop);

    XgUndoHourGlass(XtParent(w));
    XSync(dc->dpy, TRUE);
}


void
color_pushed(w, dc, call_data)
Widget    w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
Arg wargs[2];
unsigned short bgcol;
XColor tmp;


    XtSetArg (wargs[0], XmNbackground, &bgcol);
    XtGetValues(w, wargs, 1);

    tmp.pixel = bgcol;

    XQueryColor(dc->dpy, dc->cmap, &tmp);

    dc->colors.red = tmp.red;
    dc->colors.green = tmp.green;
    dc->colors.blue = tmp.blue;

    /*XStoreColor(dc->dpy, dc->cmap, &(dc->colors));*/
    XAllocColor(dc->dpy, dc->cmap, &(dc->colors));

    update_rgb(dc);
}

int 
update_rgb (data_cell *dc)
{
char str[20];

    XmScaleSetValue(dc->sliders[COL_RED], dc->colors.red);
    dc->slider_values[COL_RED] = (float)dc->colors.red / MAX_CVAL;
    sprintf (str, "%d", (int)SLIDER_VAL_REAL(dc, COL_RED));
    XmTextSetString(dc->slider_txt[COL_RED], str);

    XmScaleSetValue(dc->sliders[COL_GRN], dc->colors.green);
    dc->slider_values[COL_GRN] = (float)dc->colors.green / MAX_CVAL;
    sprintf (str, "%d", (int)SLIDER_VAL_REAL(dc, COL_GRN));
    XmTextSetString(dc->slider_txt[COL_GRN], str);

    XmScaleSetValue(dc->sliders[COL_BLU], dc->colors.blue);
    dc->slider_values[COL_BLU] = (float)dc->colors.blue / MAX_CVAL;
    sprintf (str, "%d", (int)SLIDER_VAL_REAL(dc, COL_BLU));
    XmTextSetString(dc->slider_txt[COL_BLU], str);

}


int 
set_color_prompts (void)
{

    sprintf(col_prompt[VECT_CELL], "VECTOR COLOR");
    sprintf(col_prompt[SITES_CELL], "SITES COLOR");
    sprintf(col_prompt[BG_CELL], "BACKGROUND COLOR");
    sprintf(col_prompt[SURF_CELL], "SURFACE COLOR");
    sprintf(col_prompt[GRID_CELL], "GRID COLOR");
    sprintf(col_prompt[TMP_CELL], "OTHER COLOR");
}

/* makes a 45 color palette */
int 
set_palette_colors (Display *display, Colormap cmap)
{
int ret, i, j, rows, r, g, b, gray, ramp;
static int first=1;

    if(first){  /* make sure only done once */

	gray = i = j = ramp = 0;

	for (r = 4; r < 16; r+=5){
	    for (g = 4; g < 16; g+=5){
		for (b = 4; b < 16 && i < 40; b+=5){
		    palcol[i].red = HEXVAL(r);
		    palcol[i].green = HEXVAL(g);
		    palcol[i++].blue = HEXVAL(b);
		}
		/* fourth (red to yellow to white)
		& fifth (cyan to blue to black) rows */
		if(ramp == 0){
		    palcol[i].red = MAX_CVAL;
		    palcol[i].green = 0; 
		    palcol[i++].blue = 0;
		    ramp ++;
		    palcol[i].red = 0; 
		    palcol[i].green = MAX_CVAL;
		    palcol[i++].blue = MAX_CVAL;
		    ramp ++;
		}
		else if(ramp < 10){
		    palcol[i].red= MAX_CVAL; 
		    palcol[i].green= ramp/9. * MAX_CVAL; 
		    palcol[i++].blue= 0; 
		    ramp++;
		    palcol[i].red = 0; 
		    palcol[i].green= (1. - ramp/9.) * MAX_CVAL; 
		    palcol[i++].blue = MAX_CVAL;
		    ramp++;
		}
		else if(ramp < 16){
		    palcol[i].red= MAX_CVAL; 
		    palcol[i].green= MAX_CVAL; 
		    palcol[i++].blue= (ramp - 10)/7. * MAX_CVAL; 
		    ramp++;
		    palcol[i].red = 0; 
		    palcol[i].green= 0; 
		    palcol[i++].blue = (1. - (ramp - 10)/7.) * MAX_CVAL;
		    ramp++;
		}
	    }
	}
	/* 5 gray levels */
	for (gray = 0; gray < 5; gray++){
	    palcol[i].red = palcol[i].green = palcol[i].blue = 
		    (1. - gray/4.) * MAX_CVAL;
	    i++;
	}


	for (i = 0; i < 45 ; i++){
	    palcol[i].flags = DoRed | DoGreen | DoBlue;
	    ret = XAllocColor(display, cmap, &(palcol[i]));
	    if(!ret)
		fprintf(stderr,"AllocColor failed on color %d\n", i);
	}

	first = 0;
    }	

}




