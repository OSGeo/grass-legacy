
/* primary_controls:
** makes the controls that are prevalent through out the program
*/

#include "interface.h"

/*#ifdef USE_GLX*/
/*
#include <X11/Xirisw/GlxMDraw.h>
#include <GL/GLwDraw.h>
#include <X11/GLW/GlwMDrawingArea.h>
*/
#include <GL/GLwMDrawA.h>
#include <GL/GLwDrawA.h>
/*#endif*/

/**********************************************
* create a list of items for sub menu pane  ***
**********************************************/

/********************************
* describe pulldown pane one   **
********************************/
static xman_menu_struct MenuData[]={
/*
  {" New Color File... ",before_pops,NULL,NULL,0,NULL},
  {" New Elevation File... ",before_pops,NULL,NULL,0,NULL},
  {" New Vector File... ",before_pops,NULL,NULL,0,NULL},
  {" New Sites File... ",before_pops,NULL,NULL,0,NULL},
  {NULL,NULL,NULL,NULL,0,NULL},
*/
  {" Load 3D Settings... ",NULL,NULL,NULL,0,NULL},
  {" Save 3D Settings... ",NULL,NULL,NULL,0,NULL},

  {" Image Dump... ",pop_imgdump,NULL,NULL,0,NULL},
  {" Quit ",exit_now,NULL,NULL,0,NULL}
};

/********************************
* describe pulldown pane three **
********************************/
static xman_menu_struct Menu3Data[]={
  {" Lights... ",light_panel,NULL,NULL,0,NULL},
  {" Surfaces... ",surface_panel,NULL,NULL,0,NULL},
  {" Vectors... ",vector_panel,NULL,NULL,0,NULL},
  {" Sites... ",sites_panel,NULL,NULL,0,NULL},
  {" Colors... ",color_panel,NULL,NULL,0,NULL},
  {" Cutting Planes... ",cplane_panel,NULL,NULL,0,NULL},
  {" Scripting... ",NULL,NULL,NULL,0,NULL},
  {" What's Here?... ",whats_here_panel,NULL,NULL,0,NULL},
  {" Animation... ",animation_panel,NULL,NULL,0,NULL}
};
/*
  {" Vectors... ",nyi_pops,NULL,NULL,0,NULL},
  {" Sites... ",sites_panel,NULL,NULL,0,NULL},
  {" What's Here?... ",whats_here_panel,NULL,NULL,0,NULL},
  
  {" Animation... ",animation_panel,NULL,NULL,0,NULL},
*/


/**************************************************************************
* Describe the menu bar, giving only the name to appear in the menu bar ***
* and pointers to each pulldown pane.                                   ***
**************************************************************************/
static xman_menu_struct PulldownData[] = {
	{"File    ",NULL,NULL,NULL,NULL,NULL},
	{"Panels",NULL,NULL,NULL,NULL,NULL}
};

/********************************
define the glxconfig resource *
********************************/
#ifdef USE_GLX
    static int glxConfig[] = { 
	GLX_RGBA, GLX_DOUBLEBUFFER,
	GLX_DEPTH_SIZE, 16,
	GLX_RED_SIZE, 5, GLX_GREEN_SIZE, 6,
	GLX_BLUE_SIZE, 5, GLX_ALPHA_SIZE, 0, None};

#endif


void 
make_primary_controls (data_cell *dc, int argc, char *argv[])
{	

    Arg wargs[30];
    int n;
    XGCValues values;
    Widget tmp_rc, tmp;
    int width, height;
    char *p;
    XVisualInfo *visinfo;

    PulldownData[0].sub_menu = MenuData;
    PulldownData[1].sub_menu = Menu3Data;

    PulldownData[0].n_sub_items = XtNumber(MenuData);
    PulldownData[1].n_sub_items = XtNumber(Menu3Data);

    Menu3Data[0].data = dc;
    Menu3Data[1].data = dc;
    Menu3Data[2].data = dc;
    Menu3Data[3].data = dc;
    Menu3Data[4].data = dc;
    Menu3Data[5].data = dc;
    /*
    Menu3Data[6].data = dc;
    */
    Menu3Data[7].data = dc;
    Menu3Data[8].data = dc;

    MenuData[0].data = dc;
    MenuData[1].data = dc;
    MenuData[2].data = dc;
    MenuData[3].data = dc;
    MenuData[4].data = dc; /* Quit */

    
/*--------------------------------------------------------------------------
- Define the layout and create the interface to default to full           -- 
- screen size the variable frm is the main container widget in the layout --
--------------------------------------------------------------------------*/
    dc->screen_size = XtScreen(dc->toplevel); 
    dc->dpy = XtDisplay(dc->toplevel);
    dc->scr = DefaultScreen(dc->dpy);
    dc->cmap = DefaultColormap(dc->dpy, dc->scr);
    
    width = dc->screen_size->width;
    if (p = getenv ("X3D_WIDTH"))
	width = atoi(p) < width ? atoi(p):width ;
    height = dc->screen_size->height;
    if (p = getenv ("X3D_HEIGHT"))
	height = atoi(p) < height ? atoi(p):height ;

    load_colors(dc); 

    n = 0;
    XtSetArg(wargs[n], XtNheight, height); n++;
    XtSetArg(wargs[n], XtNwidth, width); n++;
    dc->frm = XtCreateManagedWidget("form",xmFormWidgetClass,
		     dc->toplevel,wargs,n);
    
    
/*----------------------------------------------------------------------
- Define the layout of and create the main control form and its frame -- 
- this is the widget where the main controls will be                  --
----------------------------------------------------------------------*/
    
    n = 0;
    SetPositionArgs(wargs, &n, 0, 40, 65, -1, XmATTACH_FORM);
    XtSetArg(wargs[n],XmNshadowThickness,1); n++;
    XtSetArg(wargs[n],XmNshadowType,XmSHADOW_OUT); n++;
    dc->frame_for_main_control_panel = 
	    XtCreateManagedWidget("main_control_panel_frame", 
	    xmFrameWidgetClass, dc->frm, wargs, n);
    dc->form_for_main_control_panel = 
	    XtCreateManagedWidget("main_control_panel_form", 
	    xmFormWidgetClass, dc->frame_for_main_control_panel, wargs, n); 


/*--------------------------------------------------------------------
- Define the layout of and create the menubar                       --
--------------------------------------------------------------------*/
/*  ORIGINAL
    n = 0;
    SetPositionArgs(wargs, &n, 0, 4, 0, 20, NULL);
    XtSetArg(wargs[n], XmNsaveUnder, TRUE);
    dc->menubar = XmCreateMenuBar(dc->frm, "menubar",wargs,n);
*/

    n = 0;
    SetPositionArgs(wargs, &n, 0, 12, 0, 100, XmATTACH_FORM);
    XtSetArg(wargs[n], XmNsaveUnder, TRUE);
    dc->menubar = XmCreateMenuBar(dc->form_for_main_control_panel,
			"menubar",wargs,n);


    XtManageChild(dc->menubar);


/*-----------------------------------------------------------------------
- Define the layout of and create the auxillary form and its frame     -- 
- this is a widget where one of the aux panels will be shown           --
-----------------------------------------------------------------------*/
    n = 0;
    SetPositionArgs(wargs, &n, 40, -1, 65, -1, XmATTACH_FORM);
    XtSetArg(wargs[n],XmNshadowThickness,1); n++;
    XtSetArg(wargs[n],XmNshadowType,XmSHADOW_OUT); n++;
    dc->frame_for_aux_control_panel = 
	    XtCreateManagedWidget("aux_control_panel_frame",
	    xmFrameWidgetClass, dc->frm, wargs, n);
    dc->form_for_aux_control_panel = 
	    XtCreateManagedWidget("aux_control_panel_form", 
	    xmFormWidgetClass, dc->frame_for_aux_control_panel, wargs, n); 
/*
    n = 0;
    XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_POSITION); n++;
    XtSetArg(wargs[n],XmNtopPosition,50); n++;
    XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_NONE); n++;
    XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_FORM); n++;
    XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_FORM); n++;
    XtCreateManagedWidget("sep",xmSeparatorWidgetClass,
	    dc->form_for_aux_control_panel, wargs,n);

---- Define the layout of and create the redraw buttons ---------*/
    n = 0;
    XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_POSITION); n++;
/* ORIGINAL
    XtSetArg(wargs[n],XmNtopPosition,1); n++;
*/
    XtSetArg(wargs[n],XmNtopPosition,12); n++;

    XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_NONE); n++;
    XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_FORM); n++;
    XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_FORM); n++;
    XtCreateManagedWidget("REDRAW",xmLabelGadgetClass,
	    dc->form_for_main_control_panel, wargs,n);

    n = 0;/*
    SetPositionArgs(wargs, &n, 10, -1, 1, -1, XmATTACH_NONE);
    */ 
    SetPositionArgs(wargs, &n, 12, -1, 1, -1, XmATTACH_NONE);
    XtSetArg(wargs[n],XmNset,TRUE); n++;
    XtSetArg(wargs[n],XmNindicatorSize,12); n++;
    XtSetArg(wargs[n],XmNmarginHeight, 0); n++;
    dc->toggle_id[AUTO_CLEAR] = XtCreateManagedWidget("Auto Clear", 
	    xmToggleButtonWidgetClass, 
	    dc->form_for_main_control_panel, wargs,n);

    n = 0;
    SetPositionArgs(wargs, &n, 11, -1, 75, 99, XmATTACH_NONE);
    XtSetArg(wargs[n], XmNlabelString, XmStringCreateSimple("Clear")); n++;
    tmp = XtCreateManagedWidget("clear_0", xmPushButtonWidgetClass, 
	    dc->form_for_main_control_panel, wargs,n);	
    XtAddCallback(tmp, XmNactivateCallback, do_clear, dc);


    n = 0;
    SetPositionArgs(wargs, &n, 20, -1, 1, 25, XmATTACH_NONE);
    XtSetArg(wargs[n], XmNlabelString, XmStringCreateSimple("Surface")); n++;
    dc->redraw_buttons[0]= XtCreateManagedWidget("redraw_0", 
	    xmPushButtonWidgetClass, dc->form_for_main_control_panel, wargs,n);	
    XtAddCallback(dc->redraw_buttons[0], XmNactivateCallback, do_draw, dc);
    
    n = 0;
    SetPositionArgs(wargs, &n, 20, -1, 25, 50, XmATTACH_NONE);
    XtSetArg(wargs[n], XmNlabelString, XmStringCreateSimple("Vectors")); n++;
    dc->redraw_buttons[1]= XtCreateManagedWidget("redraw_1", 
	    xmPushButtonWidgetClass, dc->form_for_main_control_panel, wargs,n);	
    XtAddCallback(dc->redraw_buttons[1], XmNactivateCallback, do_vectdraw, dc);
    
    n = 0;
    SetPositionArgs(wargs, &n, 20, -1, 50, 75, XmATTACH_NONE);
    XtSetArg(wargs[n], XmNlabelString, XmStringCreateSimple("Sites")); n++;
    dc->redraw_buttons[2]= XtCreateManagedWidget("redraw_2", 
	    xmPushButtonWidgetClass, dc->form_for_main_control_panel, wargs,n);	
    XtAddCallback(dc->redraw_buttons[2], XmNactivateCallback, do_sitedraw, dc);
    
    n = 0;
    SetPositionArgs(wargs, &n, 20, -1, 75, 99, XmATTACH_NONE);
    XtSetArg(wargs[n], XmNlabelString, XmStringCreateSimple("Cancel")); n++;
    dc->redraw_buttons[3]= XtCreateManagedWidget("redraw_3", 
	    xmPushButtonWidgetClass, dc->form_for_main_control_panel, wargs,n);	
    XtAddCallback(dc->redraw_buttons[3], XmNactivateCallback, cxl_draw, dc);
    
     
    n = 0;
    XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_POSITION); n++;
    XtSetArg(wargs[n],XmNtopPosition,31); n++;
    XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_NONE); n++;
    XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_FORM); n++;
    XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_FORM); n++;
    XtCreateManagedWidget("xx",xmSeparatorGadgetClass, 
	    dc->form_for_main_control_panel, wargs,n);
    
/*--------------------------------------------------------------------
- Define the layout of and create the main control sliders          --
--------------------------------------------------------------------*/
    n = 0;
    SetPositionArgs(wargs, &n, 82, 97, 2, 40, NULL);
    dc->Mper_rc = XtCreateManagedWidget("perrc", xmRowColumnWidgetClass, 
	    dc->form_for_main_control_panel, wargs, n);

    n = 0;
    XtSetArg(wargs[n],XmNscaleHeight,15); n++;
    XtSetArg(wargs[n],XmNscaleWidth,130); n++;	
    dc->sliders[MAIN_PSP] = make_slider(dc->Mper_rc,
	    10000, 0, 500, 1, "Perspective",
	    update_sliders,update_sliders, dc,1, XmHORIZONTAL, wargs,n);
    XtAddCallback(dc->sliders[MAIN_PSP], XmNdragCallback, change_persp, dc);
    XtAddCallback(dc->sliders[MAIN_PSP], XmNvalueChangedCallback, 
	    change_persp, dc);
	    
    n = 0;
    SetPositionArgs(wargs, &n, 88, 98, 39, -1, XmATTACH_NONE);
    dc->slider_txt[MAIN_PSP] = make_text(dc->form_for_main_control_panel,
	    "", 5,"per_txt", update_sliders, dc,wargs,n);
    XtAddCallback(dc->slider_txt[MAIN_PSP], XmNactivateCallback,
	    change_persp, dc);
	    
	    
/*-- Height Slider --*/	
    n = 0;
    SetPositionArgs(wargs, &n, 50, 98, 40, 50, NULL);
    dc->Mhgt_rc = XtCreateManagedWidget("hgtrc", xmRowColumnWidgetClass,
	    dc->form_for_main_control_panel, wargs, n);


    n = 0;	
    XtSetArg(wargs[n],XmNscaleHeight,90); n++;
    XtSetArg(wargs[n],XmNscaleWidth,15); n++;	
    XtSetArg(wargs[n],XmNscaleMultiple,10); n++;
    dc->sliders[MAIN_HGT] = make_slider(dc->Mhgt_rc, 10000, 0, 500, 2, 
	    "height",update_sliders,update_sliders, dc,0,
	    XmVERTICAL,wargs,n);
    XtAddCallback(dc->sliders[MAIN_HGT], XmNdragCallback, change_height, dc);
    XtAddCallback(dc->sliders[MAIN_HGT], XmNvalueChangedCallback, 
	    change_height, dc);
    
    n = 0;
    SetPositionArgs(wargs, &n, 33, -1, 41, -1, XmATTACH_NONE);
    XtCreateManagedWidget("Height", xmLabelGadgetClass,
	    dc->form_for_main_control_panel, wargs, n);

    n = 0;
    SetPositionArgs(wargs, &n, 40, 50, 40, 57, XmATTACH_NONE);
    dc->slider_txt[MAIN_HGT] = make_text(dc->form_for_main_control_panel,
	    "", 8,"hgt_txt", update_sliders, dc ,wargs,n);
    XtAddCallback(dc->slider_txt[MAIN_HGT], XmNactivateCallback,
	    change_height, dc);
    
    
/*-- Zexag Slider --*/	
    n = 0;
    SetPositionArgs(wargs, &n, 50, 98, 60, 71, NULL);
    dc->Mzex_rc = XtCreateManagedWidget("zexrc", xmRowColumnWidgetClass,
	    dc->form_for_main_control_panel, wargs, n);
    
    n = 0;
    XtSetArg(wargs[n],XmNscaleHeight,110); n++;
    XtSetArg(wargs[n],XmNscaleWidth,15); n++;	
    XtSetArg(wargs[n],XmNscaleMultiple,10); n++;
    dc->sliders[MAIN_ZEX] = make_slider(dc->Mzex_rc, 10000, 0, 500, 5,
	    "Z-Exaggeration",update_sliders,update_sliders, dc,0,
	    XmVERTICAL,wargs,n);
    XtAddCallback(dc->sliders[MAIN_ZEX], XmNdragCallback, change_exag, dc);
    XtAddCallback(dc->sliders[MAIN_ZEX], XmNvalueChangedCallback, 
	    change_exag, dc);
    
    n = 0;
    SetPositionArgs(wargs, &n, 33, -1, 58, -1, XmATTACH_NONE);
    XtCreateManagedWidget("Z-Exag", xmLabelGadgetClass,
	    dc->form_for_main_control_panel, wargs, n);
    
    n = 0;
    SetPositionArgs(wargs, &n, 40, 50, 58, 75, XmATTACH_NONE);
    dc->slider_txt[MAIN_ZEX] = make_text(dc->form_for_main_control_panel,
	    "", 8,"zex_txt", update_sliders, dc,wargs,n);
    XtAddCallback(dc->slider_txt[MAIN_ZEX], XmNactivateCallback,
	    change_exag, dc);

#ifdef OLD
/* now in pop_transl.c */
   
/* -- Z separation Slider -- */

    n = 0;
    SetPositionArgs(wargs, &n, 35, 70, 57, 63, NULL);
    tmp_rc = XtCreateManagedWidget("zseprc", xmRowColumnWidgetClass,
	    dc->form_for_main_control_panel, wargs, n);
    
    n = 0;
/*
    XtSetArg(wargs[n],XmNscaleHeight,100); n++;
    XtSetArg(wargs[n],XmNscaleWidth,20); n++;	
*/
    dc->sliders[MAIN_ZSEP] = make_slider(tmp_rc, 10000, 0, 0, 2,
	    "Z-Separation",update_sliders,update_sliders, dc,0,
	    XmVERTICAL,wargs,n);
    XtAddCallback(dc->sliders[MAIN_ZSEP], XmNdragCallback, change_zsep, dc);
    XtAddCallback(dc->sliders[MAIN_ZSEP], XmNvalueChangedCallback, 
	    change_zsep, dc);
    
    n = 0;
    SetPositionArgs(wargs, &n, 70, -1, 57, -1, XmATTACH_NONE);
    XtCreateManagedWidget("Z-Sep", xmLabelGadgetClass,
	    dc->form_for_main_control_panel, wargs, n);
    
    n = 0;
    SetPositionArgs(wargs, &n, 75, -1, 57, -1, XmATTACH_NONE);
    dc->slider_txt[MAIN_ZSEP] = make_text(dc->form_for_main_control_panel,
	    "", 5,"zsep_txt", update_sliders, dc,wargs,n);
    XtAddCallback(dc->slider_txt[MAIN_ZSEP], XmNactivateCallback,
	    change_exag, dc);
#endif  

/*********************************************************************
* Define the layout of and create the xy position widget            **
*********************************************************************/
    
    n = 0;
    SetPositionArgs(wargs, &n, 34, 73, 1, 40, NULL);
    dc->xy_position = make_position(dc->form_for_main_control_panel, 
	    "xy-position", trackmouse,trackmouse,NULL,lgtexp,dc ,wargs,n);
    XtAddEventHandler(dc->xy_position, ButtonPressMask | Button1MotionMask,
		    FALSE, change_xypos, dc);

    n = 0;
    SetPositionArgs(wargs, &n, 74, 82, 1, 40, NULL);
    XtCreateManagedWidget("X-Y Position", xmLabelGadgetClass,
	    dc->form_for_main_control_panel, wargs, n);
    
/*********************************************************************
* Define the layout of and create the box that surrounds the        **
* where to look options                                             **
*********************************************************************/
    
    n = 0;
    SetPositionArgs(wargs, &n, 33, -1, 82, -1, XmATTACH_NONE);
    XtCreateManagedWidget("Look:",xmLabelGadgetClass, 
	    dc->form_for_main_control_panel, wargs,n);
    
    n = 0;
    SetPositionArgs(wargs, &n, 40, -1, 80, 97, XmATTACH_NONE);
    XtCreateManagedWidget("underscore",xmSeparatorGadgetClass, 
	    dc->form_for_main_control_panel, wargs,n);
    

/******************************************************************
* define the layout for and create the buttons used to determine **	
* where the viewer wants to look                                 **
******************************************************************/
    
    n = 0;
    SetPositionArgs(wargs, &n, 42, 52, 80, 97, NULL);
    dc->look[0] = XtCreateManagedWidget("Here",xmPushButtonWidgetClass, 
	    dc->form_for_main_control_panel, wargs,n);
    dc->toggle_id[LOOK_HERE] = dc->look[0];
    XtAddCallback(dc->look[0], XmNactivateCallback, add_look, dc);
    
    n = 0;
    SetPositionArgs(wargs, &n, 54, 64, 80, 97, NULL);
    dc->look[1] = XtCreateManagedWidget("Center",xmPushButtonWidgetClass, 
	    dc->form_for_main_control_panel, wargs,n);
    dc->toggle_id[LOOK_CENTER] = dc->look[1];
    XtAddCallback(dc->look[1], XmNactivateCallback, nyi_pops, dc);
    
    n = 0;
    SetPositionArgs(wargs, &n, 66, 76, 80, 97, NULL);
    dc->look[2] = XtCreateManagedWidget("Cancel",xmPushButtonWidgetClass, 
	    dc->form_for_main_control_panel, wargs,n);
    dc->toggle_id[LOOK_CANCEL] = dc->look[2];
    XtAddCallback(dc->look[2], XmNactivateCallback, nyi_pops, dc);
    

/**************************************
* define and create the reset button **
**************************************/

	    
    n = 0;
    SetPositionArgs(wargs, &n, 90, 99, 80, 97, NULL);
    dc->reset = XtCreateManagedWidget("Reset", xmPushButtonWidgetClass, 
	    dc->form_for_main_control_panel, wargs,n);	
    dc->toggle_id[MAIN_RESET] = dc->reset;
/*
    XtAddCallback(dc->reset, XmNactivateCallback, toggle, dc);
Had to change the callback to nyi_pops. The reset callback was seg-faulting and
 I couldn't figure out why.
*/

 XtAddCallback(dc->reset, XmNactivateCallback, nyi_pops, dc);
  
/*--------------------------------------------------------------------
- Define the layout of and create the monitor and its frame         --
- this is the widget where all of the drawing will take place       --
--------------------------------------------------------------------*/
    
    n = 0;
    SetPositionArgs(wargs, &n, 4, -1, 0, 65, XmATTACH_FORM);
    XtSetArg(wargs[n],XmNshadowThickness,9); n++;
    XtSetArg(wargs[n],XmNshadowType,XmSHADOW_ETCHED_OUT); n++;
    dc->monitor_frame = XtCreateManagedWidget("monitor_frame",
	    xmFrameWidgetClass, dc->frm,wargs,n);
    
#ifdef USE_GLX
    n = 0;
    /*
    XtSetArg(wargs[n], GlxNglxConfig, glxConfig); n++;
    dc->monitor = GlxCreateMDraw(dc->monitor_frame, "monitor", wargs, n);
    XtAddCallback(dc->monitor, GlxNginitCallback, init,dc);
    XtAddCallback(dc->monitor, GlxNexposeCallback, exposed,dc);
    XtAddCallback(dc->monitor, GlxNresizeCallback, resized,dc);
    XtManageChild(dc->monitor);
    */

    XtSetArg(wargs[n], GLwNattribList, glxConfig); n++;

    dc->monitor = GLwCreateMDrawingArea(dc->monitor_frame, "monitor", wargs, n);

    XtAddCallback(dc->monitor, GLwNginitCallback, init,dc);
    XtAddCallback(dc->monitor, GLwNexposeCallback, exposed,dc);
    XtAddCallback(dc->monitor, GLwNresizeCallback, resized,dc);
    XtManageChild(dc->monitor);

#else
    dc->monitor = XmCreateDrawingArea(dc->monitor_frame,"monitor",wargs,n); 
#endif
  

/*--------------------------
create a gc for drawing --
--------------------------*/
    values.foreground = WhitePixel(dc->dpy, XtWindow(dc->monitor));
    values.background = BlackPixel(dc->dpy, XtWindow(dc->monitor));
    dc->gc = XCreateGC(dc->dpy, RootWindow(dc->dpy,DefaultScreen(dc->dpy)),
	    GCForeground | GCBackground, &values);
    
/*--------------------------------------------------------------------
- Define the layout of and create the status bar                    --
--------------------------------------------------------------------*/
    n = 0;
    SetPositionArgs(wargs, &n, 1, 3, 2, 63, NULL);
    XtSetArg(wargs[n],XmNshadowThickness,1); n++;
    XtSetArg(wargs[n],XmNshadowType,XmSHADOW_IN); n++;
    dc->status_frame = XtCreateManagedWidget("status_frame", xmFrameWidgetClass, 
	    dc->frm, wargs,n);

    n = 0;
    XtSetArg(wargs[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
    dc->status = XtCreateManagedWidget("Done", xmLabelWidgetClass, 
	    dc->status_frame, wargs, n);
     
    xman_create_menu_items(NULL,dc->menubar,PulldownData,
	    (XtNumber(PulldownData)));

    XtRealizeWidget(dc->toplevel);	

    init_inform();
    set_main_sliders(dc);
    GS_set_global_exag((float)SLIDER_VAL_REAL(dc, MAIN_ZEX));
    update_ranges(dc);

    installcolormap(dc);  

    surface_panel(dc->toplevel, dc, NULL);

    enable_cxl(dc->redraw_buttons[3]);

}


void 
exit_now (Widget w, data_cell *dc, caddr_t ignored)
{
    exit(0);
}


#define NUM_MSLIDERS 3

int 
set_main_sliders (data_cell *dc)
{
int i;
Arg wargs[2];
int sl_max, sl_min, sl_range, ival, sl_id[NUM_MSLIDERS];
char str[20];

    sl_id[0] = MAIN_PSP;
    sl_id[1] = MAIN_HGT;
    sl_id[2] = MAIN_ZEX;

    for(i=0; i < NUM_MSLIDERS; i++){
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
