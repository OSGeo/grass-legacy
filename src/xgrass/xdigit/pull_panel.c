 
/*
** Written Summer, 1992 by Bill Brown
** US Army Construction Engineering Research Lab
** Modified for X, Terry Baker Spring 1993
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/

#include "digit.h"
#include "wind.h"
#include "color.h"
#include "linkm.h"

static double Centroid[2];


#include "gis.h"
#include <math.h>

#define X 0
#define Y 1



/**********************************************************************/

init_pull()
{
    Pi = 4.0 * atan(1.0);
    Cur_Points = Vect_new_line_struct();    
    Cur_Line = 0;
    Anchors_set = 0;
    pullfunc = (Dfunc_ptr *)pull_spl1;
    fillfunc = (Func_ptr *)fill_pbuf_orig;
    PLtoken = NULL;
    NewPL = 0;
    Pulled = 0;
    EditChanges = 0;
    Editing = 0;
    CurrPL = NULL;
}

/**********************************************************************/
/************************************************************************/

void
_drawcurrentfunc()
{
    int i;
    float incr;
    XPoint pt[21];
    Dimension width, height;
    int x, y, w, h;
    Pixel fg, bg;

    
    
    XtVaGetValues (Acurfunc, XtNwidth, &width, XtNheight, &height, 
			     XtNforeground, &fg, XtNbackground, &bg,
			     NULL);
    XSetForeground (dpy, gc, bg);
    XFillRectangle(dpy, XtWindow(Acurfunc), gc, 0, 0, width, height);
    XSetForeground (dpy, gc, fg);
    
    w = width;
    h = height;
    
    for(i = 0; i <= 10; i++){
	incr = i * .05;
	pt[i].x =  w * incr;

	pt[i].y = h - 0.95 * h * pullfunc(1.0 - 2.0 * incr);
    }
    for(i = 10; i <= 20; i++){
	incr = i * .05;
	pt[i].x = w * incr;
	pt[i].y = h - 0.95 * h * pullfunc(2.0 * (incr-0.5));
    }
    XDrawLines (dpy, XtWindow (Acurfunc), gc, pt, i, CoordModeOrigin);

}

/************************************************************************/
show_pull(w, pull)
    Widget w, pull;
{
static int first = 1;
    if (first)
    {
	XtVaSetValues (pull, XtNx, Winx + Wdth - 350, XtNy, Winy, NULL);
	first = 0;
    }
    if (XtIsManaged (pull))
    {
	check_changes();
	XtUnmanageChild (pull);
	close_pl();
    }
    else
    {
	XtManageChild ( pull);
    }

}
Widget
install_control_panel ()
{
    Widget form, rc, rc2, frame;
    int x, y;
    XmString title;
    Widget pull_panel;

    pull_panel = XtVaCreatePopupShell ("Edit panel", transientShellWidgetClass,
						toplevel, NULL, 0);

    form = XtVaCreateManagedWidget ("editform", xmFormWidgetClass, pull_panel,
	XmNwidth, 350,
	XmNheight, 575,
	NULL);

    /* pickanchors  */
    x = 5;
    y = 5;
    Apickanchors = XtVaCreateManagedWidget ("Pick anchor points", 
        xmPushButtonWidgetClass, form,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, x,
        XmNtopAttachment, XmATTACH_POSITION,
        XmNtopPosition, y, NULL);
    XtAddCallback (Apickanchors, XmNactivateCallback, set_anchor_points, NULL);
                         

    /* pull */
    x = 5;
    y = 15;
    Apull = XtVaCreateManagedWidget ("Pull", 
        xmPushButtonWidgetClass, form,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, x,
        XmNrightAttachment, XmATTACH_POSITION,
        XmNrightPosition, 35,
        XmNtopAttachment, XmATTACH_POSITION,
        XmNtopPosition, y, NULL);
    XtAddCallback (Apull, XmNactivateCallback, do_pull, NULL);

    /* sculpt */
    x = 5;
    y = 20;
    Asculpt= XtVaCreateManagedWidget ("Sculpt", 
        xmPushButtonWidgetClass, form,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, x,
        XmNrightAttachment, XmATTACH_POSITION,
        XmNrightPosition, 35,
        XmNtopAttachment, XmATTACH_POSITION,
        XmNtopPosition, y, NULL);
    XtAddCallback (Asculpt, XmNactivateCallback, do_sculpt, NULL);

  /* sculptsiz */

    x = 40;
    y = 20;
    Asculptsiz = 
    make_slider (form, NULL, x +1, y +1, 125, 20, 50, 1, 100, XmHORIZONTAL);

    /* blot */
    x = 5;
    y = 25;
    Ablot= XtVaCreateManagedWidget ("Blot", 
        xmPushButtonWidgetClass, form,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, x,
        XmNrightAttachment, XmATTACH_POSITION,
        XmNrightPosition, 35,
        XmNtopAttachment, XmATTACH_POSITION,
        XmNtopPosition, y, NULL);
    XtAddCallback (Ablot, XmNactivateCallback, do_blot, NULL);

    /* blotsiz */
    x = 40;
    y = 25;
    Ablotsiz = 
    make_slider (form, NULL, x +1, y +1, 125, 20, 50, 1, 100, XmHORIZONTAL);
    
    /* anti_gravity */
    x = 5;
    y = 30;
    Aantigrav= XtVaCreateManagedWidget ("Anti-gravity", 
        xmToggleButtonWidgetClass, form,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, x,
        XmNtopAttachment, XmATTACH_POSITION,
        XmNtopPosition, y, NULL);

    /* gravextent */
    x = 40;
    y = 30;
    Agravextent = 
    make_slider (form, NULL, x +1, y +1, 125, 20, 300, 100, 500, XmHORIZONTAL);


    /* showpts */
    x = 5;
    y = 35;
    Ashowpts = XtVaCreateManagedWidget ("Show points", 
        xmToggleButtonWidgetClass, form,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, x,
        XmNtopAttachment, XmATTACH_POSITION,
        XmNtopPosition, y, NULL);

    /* whole line */
    x = 5;
    y = 40;
    Awhole = XtVaCreateManagedWidget ("Whole Line", 
        xmToggleButtonWidgetClass, form,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, x,
        XmNtopAttachment, XmATTACH_POSITION,
        XmNtopPosition, y, NULL);
    XtAddCallback (Awhole, XmNvalueChangedCallback, do_whole_line, NULL);

    /* scale */
    x = 5;
    y = 45;
    Ascale = 
    make_slider (form, "Scale", x, y, 125, 20, 50, 1, 100, XmHORIZONTAL);
    XtAddCallback(Ascale, XmNvalueChangedCallback, scale_cb, NULL);
    XtAddCallback(Ascale, XmNdragCallback, scale_cb, NULL);
    

    /* cleanres */
    x = 55;
    y = 45;
    Acleanres = 
    make_slider (form, "Clean", x, y, 125, 20, 25, 1, 100, XmHORIZONTAL);
    
    /* clean */
    x = 50;
    y = 45;
    Aclean= XtVaCreateManagedWidget (" ", 
        xmToggleButtonWidgetClass, form,
	XmNindicatorSize, 15,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, x,
        XmNrightAttachment, XmATTACH_WIDGET,
        XmNrightWidget, Acleanres,
        XmNtopAttachment, XmATTACH_POSITION,
        XmNtopPosition, y, 
	NULL);
    

    /* fillfunc1 */

    x = 50;
    y = 55;
    
    rc2 = XtVaCreateManagedWidget ("rc2", xmRowColumnWidgetClass, form,
	XmNradioBehavior, True,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, x,
        XmNtopAttachment, XmATTACH_POSITION,
        XmNtopPosition, y, NULL);
    

    Afillfunc1= XtVaCreateManagedWidget ("Original points", 
        xmToggleButtonWidgetClass, rc2,
	XmNset, True, 
	XmNindicatorType, XmN_OF_MANY, NULL);
    XtAddCallback (Afillfunc1, XmNvalueChangedCallback, set_func, NULL);

    /* fillfunc2 */

    x = 50;
    y = 60;
    Afillfunc2= XtVaCreateManagedWidget ("Line between anchors", 
        xmToggleButtonWidgetClass, rc2,
	XmNindicatorType, XmN_OF_MANY, NULL);
    XtAddCallback (Afillfunc2, XmNvalueChangedCallback, set_func, NULL);
    /* fillfunc3 */
    x = 50;
    y = 65;
 
    Afillfunc4= XtVaCreateManagedWidget ("Arc", 
        xmToggleButtonWidgetClass, rc2,
	XmNindicatorType, XmN_OF_MANY, NULL);
    XtAddCallback (Afillfunc4, XmNvalueChangedCallback, set_func, NULL);

    /* func0 */
    x = 5;
    y = 70;
    rc = XtVaCreateManagedWidget ("editrc", xmRowColumnWidgetClass, form,
	XmNradioBehavior, True,
	XmNpacking, XmPACK_COLUMN,
	XmNnumColumns, 2,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, x,
        XmNtopAttachment, XmATTACH_POSITION,
        XmNtopPosition, y, NULL);
    

    /* func1 */
    Afunc1= XtVaCreateManagedWidget ("F1", xmToggleButtonWidgetClass, rc, 
	 XmNindicatorType, XmN_OF_MANY, NULL);
    XtAddCallback (Afunc1, XmNvalueChangedCallback, set_func, NULL);

    /* func2 */
    x = 5;
    y = 65;
    Afunc2= XtVaCreateManagedWidget ("F2", 
        xmToggleButtonWidgetClass, rc,
	XmNindicatorType, XmN_OF_MANY, NULL);
    XtAddCallback (Afunc2, XmNvalueChangedCallback, set_func, NULL);

    /* func3 */
    x = 5;
    y = 70;
    Afunc3= XtVaCreateManagedWidget ("F3", 
        xmToggleButtonWidgetClass, rc,
	XmNindicatorType, XmN_OF_MANY, NULL);
    
    XtAddCallback (Afunc3, XmNvalueChangedCallback, set_func, NULL);
    Afunc0= XtVaCreateManagedWidget ("Spline", 
        xmToggleButtonWidgetClass, rc, 
	 XmNindicatorType, XmN_OF_MANY, NULL);
    XtAddCallback (Afunc0, XmNvalueChangedCallback, set_func, NULL);

    /* func4 */
    x = 50;
    y = 60;
    Afunc4= XtVaCreateManagedWidget ("F4", 
        xmToggleButtonWidgetClass, rc,
	XmNset, True, 
	XmNindicatorType, XmN_OF_MANY, NULL);
    XtAddCallback (Afunc4, XmNvalueChangedCallback, set_func, NULL);

    /* func5 */
    x = 50;
    y = 65;
    Afunc5= XtVaCreateManagedWidget ("F5", 
        xmToggleButtonWidgetClass, rc,
	XmNindicatorType, XmN_OF_MANY, NULL);
    XtAddCallback (Afunc5, XmNvalueChangedCallback, set_func, NULL);

    /* func6 */
    Afunc6 = XtVaCreateManagedWidget ("F6", 
        xmToggleButtonWidgetClass, rc,
	XmNindicatorType, XmN_OF_MANY, NULL);
    
    XtAddCallback (Afunc6, XmNvalueChangedCallback, set_func, NULL);

    /* curfunc */
    x = 55;
    y = 75;


    frame = XtVaCreateManagedWidget ("frame", 
        xmFrameWidgetClass, form,
        XmNshadowThickness, 3,
        XmNshadowType, XmSHADOW_ETCHED_IN,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, x,
        XmNtopAttachment, XmATTACH_WIDGET,
        XmNtopWidget, rc2, 
        XmNbottomAttachment, XmATTACH_POSITION,
        XmNbottomPosition, y + 15, 
        XmNrightAttachment, XmATTACH_POSITION,
        XmNrightPosition, x + 40, 
	NULL);
    Acurfunc = XtVaCreateManagedWidget ("drawarea", 
        xmDrawingAreaWidgetClass, frame, NULL);
    XtAddCallback (Acurfunc, XmNexposeCallback, _drawcurrentfunc, NULL);
    
    Afunchshape = make_slider 
    (form, NULL, x, -1, 0, 20, 50, 1, 100, XmHORIZONTAL);
    XtVaSetValues (Afunchshape,
        XmNtopAttachment, XmATTACH_WIDGET,
        XmNtopWidget, frame,
        XmNrightAttachment, XmATTACH_POSITION,
        XmNrightPosition, x + 40,
	NULL);
    XtAddCallback (Afunchshape, XmNdragCallback, set_func, NULL);
    XtAddCallback (Afunchshape, XmNvalueChangedCallback, set_func, NULL);
    
    x = 5;
    y = 95;
    Accept = XtVaCreateManagedWidget ("accept line changes", 
        xmPushButtonWidgetClass, form,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, x,
        XmNtopAttachment, XmATTACH_POSITION,
        XmNtopPosition, y, NULL);
    XtAddCallback (Accept, XmNactivateCallback, keep_line, NULL);
    /* sculptreset */
    x = 55;
    y = 95;
    Asculptreset = XtVaCreateManagedWidget ("Undo", 
        xmPushButtonWidgetClass, form,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, x,
        XmNtopAttachment, XmATTACH_POSITION,
        XmNtopPosition, y, NULL);
    XtAddCallback (Asculptreset, XmNactivateCallback, do_plreset, NULL);
    /* shapereset */
    y = 85;
    Ashapereset = XtVaCreateManagedWidget ("", 
        xmPushButtonWidgetClass, form,
	XmNwidth, 16,
	XmNheight, 16,
        XmNtopAttachment, XmATTACH_POSITION,
        XmNtopPosition, 90,
        XmNrightAttachment, XmATTACH_WIDGET,
        XmNrightWidget, Afunchshape,
	NULL);
    XtAddCallback (Ashapereset, XmNactivateCallback, set_func, NULL);
    

    /* Quit */
    x = 75;
    y = 95;
    Aquit = XtVaCreateManagedWidget ("Quit", 
        xmPushButtonWidgetClass, form,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, x,
        XmNtopAttachment, XmATTACH_POSITION,
        XmNtopPosition, y, NULL);
    XtAddCallback (Aquit, XmNactivateCallback, downcb, pull_panel);
    XtAddCallback (Aquit, XmNactivateCallback, check_changes, NULL);
    XtAddCallback (Aquit, XmNactivateCallback, close_pl, NULL);

    return (pull_panel);
}


/************************************************************************/
set_func (a)
    Widget a;
{
double x, y;
    if(a == Afunchshape)
        _drawcurrentfunc();

    if(a == Ashapereset)
    {
	XtVaSetValues (Afunchshape, XtNvalue, 50, NULL);
        _drawcurrentfunc();

    }
    if (a == Afillfunc1)
    {
	if (XmToggleButtonGetState (a))
	    fillfunc = (Func_ptr *)fill_pbuf_orig;
	return(0);
    }	
    if (a == Afillfunc2)
    {
	if (XmToggleButtonGetState (a))
	    fillfunc = (Func_ptr *)fill_pbuf_line;

	return(0);
    }	
    if (a == Afillfunc4)
    {
	if (XmToggleButtonGetState (a))
	    fillfunc = (Func_ptr *)fill_pbuf_arc2;

	return(0);
    }	
    if(a == Afunc0 || a == Afunc1 || a == Afunc2 || a == Afunc3 
		    || a == Afunc4 || a == Afunc5 || a == Afunc6)
    {
	if (XmToggleButtonGetState (a))
	{
	    if(a == Afunc0)
		pullfunc = (Dfunc_ptr *)pull_spline;

	    if(a == Afunc1)
		pullfunc = (Dfunc_ptr *)pull_line;
	    
	    if(a == Afunc2)
		pullfunc = (Dfunc_ptr *)pull_cos;
	    
	    if(a == Afunc3)
		pullfunc = (Dfunc_ptr *)pull_semi;
	    
	    if(a == Afunc4)
		pullfunc = (Dfunc_ptr *)pull_spl1;
	    
	    if(a == Afunc5)
		pullfunc = (Dfunc_ptr *)pull_spl2;
	    
	    if(a == Afunc6)
		pullfunc = (Dfunc_ptr *)pull_spl3;
            
	    _drawcurrentfunc();
	}
	return(0);
    }	

}


/************************************************************************/
/************************************************************************/

void
do_pull(w)
    Widget w;
{
    int whole;
    
    check_changes();
    do_plreset();
    whole = XmToggleButtonGetState (Awhole);
    if(Cur_Line > 0 && (Anchors_set)){
	if(!PLtoken){
	    do_newpl();
	}
	Pulled = pull_it();
    }

    else{   /* no current line selected */
	XBell (XtDisplay(w), 25);
	make_monolog(1, "First set anchors!");
    }
}

/************************************************************************/

void
do_blot(w)
{
    int whole;

    whole = XmToggleButtonGetState (Awhole);
    if(Cur_Line > 0 && (Anchors_set || whole)){
	if(!PLtoken){
	    do_newpl();
	}
	blot_it();
    }

    else{   /* no current line selected */
	XBell(XtDisplay(toplevel),50);
	make_monolog(1, "First set anchors!");
    }
}

/************************************************************************/

void
do_plreset()
{
    if(PLtoken)
    {
	if (EditChanges && CurrPL)
	{
	    display_plseg(CurrPL, CLR_ERASE);
	    highlight_line (CM->Line[Cur_Line].type, Cur_Points, Cur_Line, CM); 
	}
	    
	empty_pl (PLtoken, CurrPL);
	empty_pl (PLtoken, PLtop);
	NewPL = 1;
	PLtop = NULL;
	CurrPL = NULL;
    }
    EditChanges = 0;
    Pulled = 0;

}

/************************************************************************/
void
close_pl()
{
    if(PLtoken){
	link_cleanup (PLtoken);
    }
    PLtoken = NULL;
    PLtop = NULL;
    CurrPL = NULL;
    NewPL = 0;
    EditChanges = 0;
    if (Cur_Line)
	    display_line (CM->Line[Cur_Line].type, Cur_Points, Cur_Line, CM); 
    Cur_Line = 0;
}
/************************************************************************/
void
do_newpl()
{
    if(PLtoken){
	link_cleanup (PLtoken);
    }
    EditChanges = 0;
    PLtoken = (void *)link_init(sizeof (Pntlist));
    NewPL = 1;
    PLtop = NULL;
    CurrPL = NULL;
}
/************************************************************************/

void
do_whole_line()
{

    if(Cur_Line > 0 && Anchors_set){
	    check_changes();
	if (!PLtoken)
	    do_newpl();
	else
	    do_plreset();
    }
}

/************************************************************************/

void
set_centroid()
{
Pntlist *p;
int cnt;

    if(CurrPL){
	Centroid[X] = Centroid[1] = 0.0;

	for(cnt = 0, p = CurrPL; p != NULL; p = p->next, cnt++){
	    Centroid[0] += p->pnt[0];
	    Centroid[1] += p->pnt[1];
	}
	Centroid[0] /= cnt;
	Centroid[1] /= cnt;

    }
    else if(Cur_Line > 0 && Anchors_set){
	do_newpl();
	CurrPL = copy_line_tolist(Cur_Points, PLtoken);
	set_centroid();
    }

} 

/************************************************************************/

void 
scale_cb(w, data, cbs)
    Widget w;
    void *data;
    XmScaleCallbackStruct *cbs;
{
static int dragged = 0;
    
    if(NewPL)
    {
        if(!Pulled)
	{
	    SegIndex[1] = (SegIndex[0] + SegIndex[2])/2;
	    fill_pbuf_orig();
	    Pulled = 1;
	}
	if(XmToggleButtonGetState (Awhole))
	{
            PLtop = copy_line_tolist(Cur_Points, PLtoken);
	}
	else
	{
	    PLtop = copy_pbuf_tolist(PLtoken);
	}
	CurrPL = copy_pl (PLtoken, PLtop);
	NewPL = 0;
	display_plseg(PLtop, XD_WHITE); 
    }
    if (!dragged)
    {
	redisplay_current_edit();
	Editing = 1;
    }
    if (cbs->reason == XmCR_DRAG)
    {
	display_plscale ();
	dragged = 1;
    }
    else if (cbs->value != 50 && dragged)
    {
	pl_scale();
	XtVaSetValues (Ascale, XmNvalue, 50, NULL);
	dragged = 0;
    }
}

/************************************************************************/
void
display_plscale()
{
Pntlist *p;
int scale;
double dscale;

    set_centroid();
    copy_pix();
    XtVaGetValues (Ascale, XmNvalue, &scale, NULL);
    dscale = scale/50.0;
    if(CurrPL){
	display_plseg_scaled(CurrPL, XD_WHITE, dscale, Centroid);
    }
}

/************************************************************************/

void
pl_scale()
{
Pntlist *p;
double dir[2], scalefac;
int scale, whole;
    int x, y, w, h;
    int action = 0;

    
    set_centroid();
    if(CurrPL){
        XtVaGetValues (Ascale, XmNvalue, &scale, NULL);
        scalefac = scale/50.0;
        
        show_select_dialog("accept", "abort", "Accept scale changes?", 1);
	while (action != ACCEPT && action != DONE)
	{
	     action = Check_for_action (&x, &y);
	     if (action == ACCEPT)  /* scale point list */
	     {
		    display_plseg(CurrPL, CLR_ERASE);
		    if(XmToggleButtonGetState(Awhole))
		    {
	    		for(p = CurrPL; p != NULL; p = p->next){
			    dir[0] = p->pnt[0] - Centroid[0];
			    dir[1] = p->pnt[1] - Centroid[1];
			    p->pnt[0] = Centroid[0] + scalefac * dir[0]; 
			    p->pnt[1] = Centroid[1] + scalefac * dir[1]; 
			}
		    }
		    else
		    {
			for(p = CurrPL->next; p->next != NULL ; p = p->next)
			{
			    dir[0] = p->pnt[0] - Centroid[0];
			    dir[1] = p->pnt[1] - Centroid[1];
			    p->pnt[0] = Centroid[0] + scalefac * dir[0]; 
			    p->pnt[1] = Centroid[1] + scalefac * dir[1]; 
			}
		    }
		    PLtop = copy_pl (PLtoken, CurrPL);
		    EditChanges = 1;
            }
	    else if (action == DONE)
		    display_plseg(CurrPL, CLR_ERASE);
	    /* if action is DONE do nothing */
	} 
	Editing = 0;
        copy_pix();
	display_plseg(CurrPL, XD_WHITE);

    }	
}

/************************************************************************/

void
do_sculpt()
{
    int whole;

    whole = XmToggleButtonGetState (Awhole);
    if(Cur_Line > 0 && (Anchors_set || whole)){
	if(!PLtoken){
	    do_newpl();
	}

	sculpt_it();
    }

    else{   /* no current line selected */
	XBell(XtDisplay(toplevel),50);
	make_monolog (1,"First set anchors!");
    }

}

/************************************************************************/
/* returns 0 or -1 on error or if user didn't pick line */
int
reset_line(e, n)
double e, n;
{
int line, end;
 
    EditChanges = 0;
    if(Cur_Line > 0){   /* un-highlight */
	if(0 <= V2_read_line(CM, Cur_Points, Cur_Line))
	{
	    display_line
		    (CM->Line[Cur_Line].type, Cur_Points, Cur_Line, CM); 
	}
    }	
    line = dig_point_by_line
		    (CM, e-TRIPIX,n+TRIPIX,e+TRIPIX,n-TRIPIX,LINE | AREA);
    if (!line) 
	return -1;


    if(0 <= V2_read_line(CM, Cur_Points, line)){
	end = Cur_Points->n_points - 1;
	Cur_Line = line;	
	Closed = (Cur_Points->x[0] == Cur_Points->x[end] &&
		    Cur_Points->y[0] == Cur_Points->y[end]);

	highlight_line (CM->Line[Cur_Line].type, Cur_Points, Cur_Line, CM); 
	Anchors_set = 0;
    }
    else
	return -1;

    return 0;
}

/************************************************************************/
void
check_changes()
{
    if (Pull_flag)
    {
	if (EditChanges && CurrPL)
	    if (mouse_yes_no ("Accept previous changes as permanent?"))
        /* write out changes */
		keep_line();
	do_plreset();
    }
}
/************************************************************************/

void 
set_anchor_points()
{
int ok1=0, ok2=0, id1, id2;
double e, n;
int cmd;
int screen_x, screen_y;


    if(Anchors_set){
	erase_anchors(Anchor);
	Anchors_set = 0;
    }
   
    check_changes();
    do_plreset();
    show_select_dialog("accept", "abort", "select anchor points", 1);

    while(1)
    {
	get_location_with_pointer (&screen_x, &screen_y, &cmd);
        if (cmd == FIND)
	{
	    screen_to_utm (screen_x, screen_y, &e, &n); 
		    
	    if(ok1)
	    {
		erase_anchor(Anchor[0]);

		if (ok1)
		    erase_anchor(Anchor[1]);

		if(Cur_Line != dig_point_by_line
			(CM, e-TRIPIX,n+TRIPIX,e+TRIPIX,n-TRIPIX,LINE | AREA))
		{ 
		    if (0 > reset_line(e, n))
		    {
			Anchors_set = Cur_Line = 0;
			ok1 = ok2 = 0;
		    }
		    if (Cur_Line > 0)
		    {
			id1 = get_closest_point(Cur_Points, e, n, Anchor[FIRST]);
			if(0 <= id1){
			    display_anchor(Anchor[FIRST]);
			    ok1 = 1;
			    ok2 = 0;
			}
		    }
		}
		else
		{
		    id2 = id1;
		    swap_anchors ();
		    id1 = get_closest_point(Cur_Points, e, n, Anchor[FIRST]);
		    if(0 <= id1){
			display_anchor(Anchor[FIRST]);
			display_anchor(Anchor[SECOND]);
			ok1=1;
			ok2=2;
		    }
		}
	    }
	    else
	    {
		Cur_Line = dig_point_by_line
			(CM, e-TRIPIX,n+TRIPIX,e+TRIPIX,n-TRIPIX,LINE | AREA);

		if (0 > reset_line(e, n))
		{
		    Anchors_set = Cur_Line = 0;
		    ok1 = ok2 = 0;
		}
		if (Cur_Line > 0)
		{

		    id1 = get_closest_point(Cur_Points, e, n, Anchor[FIRST]);
		    if(0 <= id1)
		    {
			display_anchor(Anchor[FIRST]);
			ok1=1;
		    }
		}
	    }
	}
	else 
	    break;
    }



    if(ok1 && ok2 && cmd == ACCEPT){
	SegIndex[0] = id1;
	SegIndex[2] = id2;
	if(id2 - id1 < 0) swap_anchors();
	Anchors_set = 1;
	Pulled = 0;
    }
    else{
	XBell(XtDisplay(toplevel),50);
	Anchors_set = 0;
	if (ok1)
	    erase_anchor(Anchor[0]);
	if (ok2)
	    erase_anchor(Anchor[1]);
	if (Cur_Line)
	{
	    if(0 <= V2_read_line(CM, Cur_Points, Cur_Line))
	    {
		display_line
		    (CM->Line[Cur_Line].type, Cur_Points, Cur_Line, CM); 
	    }
	}

	Cur_Line = 0;
	if (cmd != DONE)
	    make_monolog(1,"Error selecting anchor points.");
    }


    XtVaSetValues (Apickanchors, XmNset, False, NULL);
}


/************************************************************************/
void
swap_anchors()
{
double tmp[2];
int itmp;

    tmp[0] = Anchor[FIRST][0];
    tmp[1] = Anchor[FIRST][1];
    itmp = SegIndex[0];

    Anchor[FIRST][0] = Anchor[SECOND][0];
    Anchor[FIRST][1] = Anchor[SECOND][1];
    SegIndex[0] = SegIndex[2];

    Anchor[SECOND][0] = tmp[0];
    Anchor[SECOND][1] = tmp[1];
    SegIndex[2] = itmp;

}


/************************************************************************/
Widget 
make_slider (parent, name, left, top, w, h, val, min, max, orient)
    Widget parent;
    char *name;
    int left, top, w, h;
    int val, min, max;
    int orient;
{
    Widget tmp;
    XmString title = NULL;	/* dpg */
    Arg wargs[10];
    int n;
   

    tmp = 
    XtVaCreateManagedWidget (name, xmScaleWidgetClass, parent,
        XmNvalue, val, 
        XmNminimum, min, 
        XmNmaximum, max,  
        XmNorientation, orient,
	NULL);
    
    n =0;
    if (name != NULL)
    {
	title = XmStringCreateSimple (name);
        XtSetArg (wargs[n], XmNtitleString, title); n++;
    }
    if (left >= 0)
    {
        XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg (wargs[n], XmNleftPosition, left); n++;
    }
    if (top >= 0)
    {
        XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
        XtSetArg (wargs[n], XmNtopPosition, top); n++;
    }
    if (w > 0)
    {
        XtSetArg (wargs[n], XmNscaleWidth, w); n++;
    }
    if (h > 0)
    {
        XtSetArg (wargs[n], XmNscaleHeight, h); n++;
    }
    
    XtSetValues (tmp, wargs, n);

    if (title)
	XtFree (title);

    return (tmp);
}


