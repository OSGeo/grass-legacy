
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/

#include "gis.h"
#include "externs.h"

char Range_txt[][12] = {
    " 10000000.0",
    " 1000000.0 ",
    " 100000.0  ",
    " 10000.0   ",
    " 1000.0    ",
    " 100.0     ",
    " 10.0      ",
    " 1.0       ",
    " 0.1       ",
    " 0.01      ",
    " 0.001     ",
    " 0.0001    ",
    " 0.00001   ",
    " 0.000001  ",
    " 0.0000001 ",
};

float Range_val[] = {
    10000000.0,
    1000000.0,
    100000.0,
    10000.0,
    1000.0,
    100.0,
    10.0,
    1.0,
    0.1,
    0.01,
    0.001,
    0.0001,
    0.00001,
    0.000001,
    0.0000001,
};

float Range_val_ex[] = {
    1000.0,
    100.0,
    100.0,
    10.0,
    10.0,
    10.0,
    10.0,
    1.0,
    0.1,
    0.01,
    0.01,
    0.01,
    0.01,
    0.001,
    0.001,
};

void update_xy ();
void update_height ();
void update_view_dir ();
void update_incl ();
void update_persp ();
void update_exag ();
void update_vectz ();
void newcell ();

Panel * my_initscriptpanel ();


#define MKACT(_a, _type,_label) \
  a=(_a)=pnl_mkact(_type);              \
  (_a)->label=_label

#define ADDACT \
  a->x=x;                       \
  a->y=(y-=a->h+dl+PNL_DIM_1);  \
  pnl_addact(a, p)

#define OVER    \
  x+=a->w+PNL_DIM_1;    \
  y+=a->h+dl+PNL_DIM_1

install_panels ()
{
    install_control_panel ();
    install_movement_panel ();
    /*install_object_panel ();*/
}

static int dummy;

void
toggle_script ()
{
    TOGGLE (P_Script->visible);
    pnl_fixpanel (P_Script);
}

install_control_panel ()
{
    Panel *panel;

    /*initscriptpanel ();*/
    P_Script = my_initscriptpanel ();
    P_Script->visible = 0;
    pnl_fixpanel (P_Script);

    if (NULL == (panel = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    panel->label = "Control";

    pnl_needredraw ();

    /* Quit */
    Aquit=pnl_mkact (pnl_wide_button);
    Aquit->label = " Quit ";
    Aquit->x = 5.5;
    Aquit->y = -0.5;
    pnl_addact (Aquit, panel);

/*
    Afull=pnl_mkact (pnl_wide_button);
    Afull->label = "Full Screen";
    Afull->y= -1.0;
    Afull->x=4.0;
    pnl_addact (Afull, panel);
*/

    /* Shading */
    Ashading=pnl_mkact (pnl_toggle_button);
    Ashading->label = " Gouraud";
    Ashading->x = 1.0;
    Ashading->y = 1.5;
    pnl_addact (Ashading, panel);

    /* Polygon */
    Apoly=pnl_mkact (pnl_toggle_button);
    Apoly->label = " Polygon";
    Apoly->x = 1.0;
    Apoly->y = 4.0;
    pnl_addact (Apoly, panel);

    /* Grid */
    Agrid=pnl_mkact (pnl_toggle_button);
    Agrid->label = " Wire";
    Agrid->x = 1.0;
    Agrid->y = 3.0;
    pnl_addact (Agrid, panel);

    /* Colored Grid */
    Agridc=pnl_mkact (pnl_toggle_button);
    Agridc->label = " Col Wire";
    Agridc->x = 1.0;
    Agridc->y = 2.5;
    pnl_addact (Agridc, panel);

    /* Grid/poly */
    Agpoly=pnl_mkact (pnl_toggle_button);
    Agpoly->label = " Wire/Poly";
    Agpoly->x = 1.0;
    Agpoly->y = 3.5;
    pnl_addact (Agpoly, panel);

    /* Vect */
    if (Vect_file)	/* only post, if a vector file exists */
    {
	Avect=pnl_mkact (pnl_wide_button);
	Avect->label = "  Vect ";
	Avect->x = 3.2;
	Avect->y = -0.5;
	pnl_addact (Avect, panel);
    }
    else
	Avect = (Actuator *) &dummy;

    /* Fringe */
    Afringe=pnl_mkact (pnl_toggle_button);
    Afringe->label = " Fringe";
    Afringe->x = 1.0;
    Afringe->y = 1.0;
    pnl_addact (Afringe, panel);

    /* Range multiplier */
    /*
    Arange=pnl_mkact (pnl_toggle_button);
    Arange->label = " Range";
    Arange->x = 1.0;
    Arange->y = 3.5;
    pnl_addact (Arange, panel);
    */

    /* Draw */
    Adraw=pnl_mkact (pnl_wide_button);
    Adraw->label = " Draw ";
    Adraw->x = 1.0;
    Adraw->y = -0.5;
    pnl_addact (Adraw, panel);

    /* Script */
    Ascript=pnl_mkact (pnl_wide_button);
    Ascript->label = " Script ";
    Ascript->x = 3.2;
    Ascript->y = -1.5;
    Ascript->downfunc = toggle_script;
    pnl_addact (Ascript, panel);

    /* New Cell */
    Anewcell=pnl_mkact (pnl_wide_button);
    Anewcell->label = " New Cell ";
    Anewcell->x = 1.0;
    Anewcell->y = -1.0;
    Anewcell->downfunc = newcell;
    pnl_addact (Anewcell, panel);

    /* Reset */
    Areset=pnl_mkact (pnl_button);
    Areset->label = " Reset";
    Areset->x = 1.0;
    Areset->y = 0.5;
    pnl_addact (Areset, panel);

    /* fast1 */
    Afast1=pnl_mkact (pnl_up_arrow_button);
    Afast1->label = " Grid Resolution";
    Afast1->x = 4.0;
    Afast1->y = 4.0;
    pnl_addact (Afast1, panel);

    /* fast2 */
    Afast2=pnl_mkact (pnl_down_arrow_button);
    Afast2->label = "";
    Afast2->x = 4.0;
    Afast2->y = 3.5;
    pnl_addact (Afast2, panel);

    /* slow1 */
    Aslow1=pnl_mkact (pnl_up_arrow_button);
    Aslow1->label = " Poly Resolution";
    Aslow1->x = 4.0;
    Aslow1->y = 2.5;
    pnl_addact (Aslow1, panel);

    /* slow2 */
    Aslow2=pnl_mkact (pnl_down_arrow_button);
    Aslow2->label = "";
    Aslow2->x = 4.0;
    Aslow2->y = 2.0;
    pnl_addact (Aslow2, panel);

    /* range1 */
    Arange1=pnl_mkact (pnl_up_arrow_button);
    Arange1->label = " Z Scale";
    Arange1->x = 4.0;
    Arange1->y = 1.0;
    pnl_addact (Arange1, panel);

    /* range2 */
    Arange2=pnl_mkact (pnl_down_arrow_button);
    Arange2->label = "";
    Arange2->x = 4.0;
    Arange2->y = 0.5;
    pnl_addact (Arange2, panel);

}

install_movement_panel ()
{
    Panel *panel;

    if (NULL == (panel = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    panel->label = "Movement";

    /* XY */
    Axy=pnl_mkact(pnl_puck);
    Axy->label="XY position";
    Axy->x=1.0;
    Axy->y=3.0;
    Axy->minval= 0.0;
    Axy->maxval=1.0;
    Axy->activefunc=update_xy;
    Axy->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(Axy, panel);

    /* height */
    Aheight=pnl_mkact(pnl_vslider);
    Aheight->label="Height";
    Aheight->x=6.0;
    Aheight->y=3.0;
    Aheight->minval= 0.0;
    Aheight->maxval=1.0;
    Aheight->activefunc=update_height;
    Aheight->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(Aheight, panel);

    /* Z exageration */
    Aexag=pnl_mkact(pnl_vslider);
    Aexag->label="Z Exag";
    Aexag->x=7.3;
    Aexag->y=3.0;
    Aexag->minval= 0.0;
    Aexag->maxval=1.0;
    Aexag->activefunc=update_exag;
    Aexag->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(Aexag, panel);

    /* Vector location */
    if (Vect_file)
    {
	Avectz=pnl_mkact(pnl_vslider);
	Avectz->label="Vect Z";
	Avectz->x=8.5;
	Avectz->y=3.0;
	Avectz->minval= 0.0;
	Avectz->maxval=1.0;
	Avectz->activefunc=update_vectz;
	Avectz->labeltype = PNL_LABEL_BOTTOM;
	pnl_addact(Avectz, panel);
    }

    /* view direction */
    Alook=pnl_mkact(pnl_dial);
    Alook->label="View Dir.";
    Alook->labeltype = PNL_LABEL_BOTTOM;
    Alook->x= 1.0;
    Alook->y= 7.5;
    Alook->minval= 0.0;
    Alook->maxval=3600.0;
    Alook->activefunc=update_view_dir;
    PNL_ACCESS (Dial, Alook, winds) = 1.;
    pnl_addact(Alook, panel);

    /* view inclination */
    Aincl=pnl_mkact(pnl_dial);
    Aincl->label="View Incline";
    Aincl->labeltype = PNL_LABEL_BOTTOM;
    Aincl->x= 4.0;
    Aincl->y= 7.5;
    Aincl->minval= -900.0;
    Aincl->maxval=  900.0;
    Aincl->activefunc=update_incl;
    PNL_ACCESS (Dial, Aincl, winds) = .5;
    pnl_addact(Aincl, panel);

    /* view Twist */
    Atwist=pnl_mkact(pnl_dial);
    Atwist->label="Twist";
    Atwist->labeltype = PNL_LABEL_TOP;
    Atwist->x= 2.5;
    Atwist->y= 7.5;
    Atwist->minval= 0.0;
    Atwist->maxval=3600.0;
    Atwist->activefunc=update_view_dir;
    PNL_ACCESS (Dial, Atwist, winds) = 1.;
    pnl_addact(Atwist, panel);

    /* Perspective */
    Apersp=pnl_mkact(pnl_hslider);
    Apersp->label="Perspective";
    Apersp->labeltype = PNL_LABEL_BOTTOM;
    Apersp->x=1.0;
    Apersp->y=1.5;
    Apersp->minval= 30.0;
    Apersp->maxval=1200.0;
    Apersp->activefunc=update_persp;
    Apersp->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(Apersp, panel);

}

install_object_panel ()
{
    Panel *panel;

    if (NULL == (panel = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    panel->label = "Movement";

    /* XY */
    AOxy=pnl_mkact(pnl_puck);
    AOxy->label="XY position";
    AOxy->x=1.0;
    AOxy->y=3.0;
    AOxy->minval= 0.0;
    AOxy->maxval=1.0;
    AOxy->activefunc=update_xy;
    AOxy->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(AOxy, panel);

    /* height */
    AOheight=pnl_mkact(pnl_vslider);
    AOheight->label="Height";
    AOheight->x=6.0;
    AOheight->y=3.0;
    AOheight->minval= 0.0;
    AOheight->maxval=1.0;
    AOheight->activefunc=update_height;
    AOheight->labeltype = PNL_LABEL_BOTTOM;
    pnl_addact(AOheight, panel);

    /* Rotate about Z */
    AOzrot=pnl_mkact(pnl_dial);
    AOzrot->label="XY Rotation";
    AOzrot->labeltype = PNL_LABEL_BOTTOM;
    AOzrot->x= 1.0;
    AOzrot->y= 7.5;
    AOzrot->minval= 0.0;
    AOzrot->maxval=3600.0;
    AOzrot->activefunc=update_view_dir;
    PNL_ACCESS (Dial, AOzrot, winds) = 1.;
    pnl_addact(AOzrot, panel);

    /* Rotate about Y */
    AOxrot=pnl_mkact(pnl_dial);
    AOxrot->label="YZ Rotation";
    AOxrot->labeltype = PNL_LABEL_BOTTOM;
    AOxrot->x= 4.0;
    AOxrot->y= 7.5;
    AOxrot->minval= 0.0;
    AOxrot->maxval=3600.0;
    AOxrot->activefunc=update_view_dir;
    PNL_ACCESS (Dial, AOxrot, winds) = 1.;
    pnl_addact(AOxrot, panel);

    /* Rotate about Y */
    AOyrot=pnl_mkact(pnl_dial);
    AOyrot->label="XZ Rotation";
    AOyrot->labeltype = PNL_LABEL_BOTTOM;
    AOyrot->x= 7.0;
    AOyrot->y= 7.5;
    AOyrot->minval= 0.0;
    AOyrot->maxval=3600.0;
    AOyrot->activefunc=update_view_dir;
    PNL_ACCESS (Dial, AOyrot, winds) = 1.;
    pnl_addact(AOyrot, panel);

    /* Bind */
    AObind=pnl_mkact (pnl_toggle_button);
    AObind->label = "Bind to Surface";
    AObind->x = 1.0;
    AObind->y = 1.0;
    pnl_addact (AObind, panel);
}

void
_update_xy ()
{
    float dx, dy;

    dx = FROM_TO[TO][X] - FROM_TO[FROM][X];
    dy = FROM_TO[TO][Y] - FROM_TO[FROM][Y];

    FROM_TO[FROM][X] = PNL_ACCESS (Point, Axy, x) * XRange + XBase;
    FROM_TO[FROM][Y] = PNL_ACCESS (Point, Axy, y) * YRange + YBase;

    FROM_TO[TO][X] = FROM_TO[FROM][X] + dx;
    FROM_TO[TO][Y] = FROM_TO[FROM][Y] + dy;
}

void
update_xy ()
{
    _update_xy ();

    update_projection ();
    do_fast_display ();
}

void
_update_height ()
{
    float dz;

    dz = FROM_TO[TO][Z] - FROM_TO[FROM][Z];

    FROM_TO[FROM][Z] = Aheight->val * ZRange + ZBase;
    /*FROM_TO[TO][Z] = FROM_TO[FROM][Z] + dz; TST*/
    FROM_TO[TO][Z] = FROM_TO[FROM][Z] + dz;  
}

void
update_height ()
{
    _update_height ();

    update_projection ();
    do_fast_display ();
}

void
_update_exag ()
{
    float dz;

    /*Z_exag = Aexag->val * 10. * Range_val_ex[Range];*/
    Z_exag = Aexag->val * 10. * Range_val[Range];
}

void
update_exag ()
{
    _update_exag ();

    update_projection ();
    do_fast_display ();
}

void
update_vectz ()
{
    float dz;

    /*Vect_z = Avectz->val * 2*Z_Span + 2*Z_Min;*/
    Vect_z = Avectz->val * 4*Z_Span;
    Vect_z -= 2*Z_Span;

    /*update_projection ();*/
    do_fast_display ();
    /*do_fast_vect_display ();*/
}

void
update_view_dir ()
{
    transform_fromto ();
    do_fast_display ();
}

void
update_incl ()
{
    transform_fromto ();
    do_fast_display ();
}

void
_update_persp ()
{
    persp = Apersp->val;
}

void
update_persp ()
{
    _update_persp ();

    update_projection ();
    do_fast_display ();
}

serve_actuator (a)
    Actuator *a;
{
    if (a == Aquit)
    {
	leave ();
    }
    if (a == Ashading)
    {
	shading = a->val;
	redraw_ok = 1;
	return (0);
    }
    if (a == Agrid)
    {
	if (Apoly->val)
	{
	    Apoly->val = 0;
	    Apoly->dirtycnt = 2;
	    pnl_fixact (Apoly);
	}
	if (Agpoly->val)
	{
	    Agpoly->val = 0;
	    Agpoly->dirtycnt = 2;
	    pnl_fixact (Agpoly);
	}
	Display_type = D_GRID;
	return (0);
    }
#ifdef FOO
    if (a == Ascript)
    {
	static int On = 0;
	/*static Panel *P_script = NULL;*/

	if (Ascript->val)
	{
	    TOGGLE (P_Script->visible);
	    pnl_fixpanel (P_Script);
	}
    }
#endif

    if (a == Agpoly)
    {
	if (Apoly->val)
	{
	    Apoly->val = 0;
	    Apoly->dirtycnt = 2;
	    pnl_fixact (Apoly);
	}
	if (Agrid->val)
	{
	    Agrid->val = 0;
	    Agrid->dirtycnt = 2;
	    pnl_fixact (Agrid);
	}
	Display_type = D_GPOLY;
	return (0);
    }
    if (a == Apoly)
    {
	if (Agrid->val)
	{
	    Agrid->val = 0;
	    Agrid->dirtycnt = 2;
	    pnl_fixact (Agrid);
	}
	if (Agpoly->val)
	{
	    Agpoly->val = 0;
	    Agpoly->dirtycnt = 2;
	    pnl_fixact (Agpoly);
	}
	Display_type = D_POLY;
	return (0);
    }
    if (a == Adraw)
    {
	do_display (Display_type);
	return (0);
    }

/*
    if (a == Afull)
    {
	winconstraints ();
	winconstraints ();
	fullscrn ();
	winopen ("Full");
	do_display (Display_type);
	return (0);
    }
*/

    if (a == Afringe)
    {
	if (Afringe->val)		/* just turned on */
	    display_fringe (X_Modr, Y_Modr);
	Fringe_on = Afringe->val;
    }
    if (a == Avect)
    {
	do_vect_display ();
	return (0);
    }
    if (a == Afast1 || a == Afast2)
    {
	if (a == Afast1)
	{
	    fast_res++;
	    if (fast_res > RES_MAX)
		fast_res = RES_MAX;
	}
	else
	{
	    fast_res--;
	    if (fast_res < 1)
		fast_res = 1;
	}
	update_fast_res ();
	X_Mod = fast_res;
	Y_Mod = fast_res;
	return (0);
    }
    if (a == Aslow1 || a == Aslow2)
    {
	if (a == Aslow1)
	{
	    slow_res++;
	    if (slow_res > RES_MAX)
		slow_res = RES_MAX;
	}
	else
	{
	    slow_res--;
	    if (slow_res < 1)
		slow_res = 1;
	}
	update_slow_res ();
	X_Modr = slow_res;
	Y_Modr = slow_res;
	return (0);
    }
    if (a == Arange1 || a == Arange2)
    {
	if (a == Arange2)
	{
	    Range++;
	    if (Range > 14)
		Range = 14;
	}
	else
	{
	    Range--;
	    if (Range < 0)
		Range = 0;
	}
	update_range();
	return (0);
    }
    if (a == Areset)
    {
	initialize2 ();
	return (0);
    }

    return (0);
}


update_fast_res ()
{
    static char fast_buf[10];

    sprintf (fast_buf, "%6d", fast_res);
    Afast2->label = fast_buf;
    Afast2->dirtycnt = 2;
    pnl_fixact (Afast2);
}

update_slow_res ()
{
    static char slow_buf[10];

    sprintf (slow_buf, "%6d", slow_res);
    Aslow2->label = slow_buf;
    Aslow2->dirtycnt = 2;
    pnl_fixact (Aslow2);
}

update_range ()
{
    static char range_buf[10];

    /*sprintf (range_buf, "%s", Range_txt[Range]);*/
    /*Arange2->label = range_buf;*/
    Arange2->label = Range_txt[Range];
    Arange2->dirtycnt = 2;
    pnl_fixact (Arange2);

    _update_zbounds ();
    _update_height ();
    _update_exag ();
    update_view_dir ();
}

/*
** this is only called when viewing position needs to be 
**  re-transformed  (i.g. for view dir or view incl)
**  simple translation does not require this to be called,
**  since that can be handled by simple addition.
**
**  FROM_TO is actuall X,Y,Z locations of eye position and point
**   in space that we are looking towards.  The base position is
**   (1000,0,0)  as given in UNIT_FROM_TO
**  This is then translated the FROM_TO[FROM] position, and then
**   rotated to get the LOOK_TO position.
*/
transform_fromto ()
{
    P_popmatrix ();
    P_pushmatrix ();
	P_rotate ((int) Aincl->val, 'y');
	P_rotate ((int) Alook->val, 'z');
	P_translate (FROM_TO[FROM][X],FROM_TO[FROM][Y],FROM_TO[FROM][Z]);
	P_transform (2, UNIT_FROM_TO, FROM_TO);

	update_projection ();
}

/*
**  update the Z_Min and Z_Max values.
*/
#ifdef ORIG
_update_zbounds ()
{
    Z_Min = Z_Min_real - Z_Span_real*5 * Range_val[Range];
    Z_Max = Z_Max_real +  Z_Span_real*10 * Range_val[Range];
    Z_Span = Z_Max - Z_Min;
    ZRange = 2 * (Z_Max - Z_Min);
}

#else
_update_zbounds ()
{
    /*Z_Min = (Z_Min_real - Z_Span_real*5) * Range_val[Range];*/
    /*Z_Max = (Z_Max_real +  Z_Span_real*10) * Range_val[Range];*/
    /*ZRange = 2 * (Z_Max - Z_Min);*/
    Z_Min = (Z_Min_real - Z_Span_real) * Range_val[Range];
    Z_Max = (Z_Max_real +  Z_Span_real) * Range_val[Range];
    Z_Span = Z_Max - Z_Min;
    ZRange = 2 * Z_Span;   /* for height */
}
#endif
