#include <stdio.h>
#include <gl.h>
#include <device.h>
#include <panel.h>

static Actuator *A_yes, *A_no, *A_quest, *A_yn_frame;
static Panel *P_yesno;
  
static Panel *init_yesno_panel();

static int Set_Up = 0;

void _fixlabel();
void _addlabel();

pnl_yesno (str)
    char *str;
{
    Actuator *a;
    int res = -1;

    if (!Set_Up)
	init_yesno_panel ();

    pnl_unselect_all (P_yesno);	/* disable all other panels */

    P_yesno->visible = 1;
    A_quest->label = str;
    pnl_setdirty (A_quest);
    pnl_setdirty (A_yn_frame);
    noborder ();
    pnl_fixpanel (P_yesno);


    while (res<0)
    {
	pnl_dopanel ();

	/* sub actuators don't seem to get returned from dopanel */
	/* so just test them directly */
	if (A_yes->val)
	{
	    res = 1;
	    a = A_yes;
	}
	else
	if (A_no->val)
	{
	    a = A_no;
	    res = 0;
	}
    }

    /* there is a bug which shows up when you make a panel
    ** that is currently active invisible, once it is invisible
    **  it does not seem to check for any events, including the
    **  mouse release of the event that was active when it when invisible.
    ** the effect is that successive calls to dopanel return the same
    ** actuator.
    ** To get around this, I wait for the actuator to go inactive before
    **  making the panel invisible.  There could be a better way, but
    **  I haven't found it.
    */
    /* wait for mouse release */
    while (a->active)
	pnl_dopanel ();

    P_yesno->visible = 0;
    pnl_fixpanel (P_yesno);

    pnl_select_all (P_yesno);	/* restore all other panels */

    return res;
}

static Panel *
init_yesno_panel()
{
    Actuator *a;
    Panel *p;

    Set_Up = 1;

    noborder ();
    P_yesno =p= pnl_mkpanel();
    p->label = "yes/no";
    p->ppu = 50.;
    p->x = XMAXSCREEN/3;
    p->y = YMAXSCREEN/2;
    p->upfunc = p->fixfunc;

    A_yn_frame = a= pnl_mkact(pnl_frame);
    PNL_ACCESS (Frame, a, mode) = PNL_FM_FREE;
    pnl_addact(a, p);

    A_yes =a= pnl_mkact(pnl_wide_button);
    a->label="YES";
    a->x = 4.;
    a->y = 1.;
    pnl_addsubact(a, A_yn_frame);

    A_no =a= pnl_mkact(pnl_wide_button);
    a->label="NO";
    a->x = 7.;
    a->y = 1.;
    pnl_addsubact(a, A_yn_frame);

    A_quest =a= pnl_mkact(pnl_label);
    a->x = 2.;
    a->y = 3.;
    /*a->label="";*/
    a->label="                                                     ";
    /*a->addfunc=_addlabel;*/
    /*a->fixfunc=_fixlabel;*/
    pnl_addsubact(a, A_yn_frame);
/*
    A_yes =a= pnl_mkact(pnl_wide_button);
    a->label="YES";
    a->x = 4.;
    a->y = 1.;
    pnl_addact(a, p);

    A_no =a= pnl_mkact(pnl_wide_button);
    a->label="NO";
    a->x = 7.;
    a->y = 1.;
    pnl_addact(a, p);

    A_quest =a= pnl_mkact(pnl_label);
    a->x = 2.;
    a->y = 3.;
    a->label="";
    a->addfunc=_addlabel;
    a->fixfunc=_fixlabel;
    pnl_addact(a, p);
*/

    p->visible = 0;

    return p;
}


/* dpg */
void
  _fixlabel(a)
Actuator *a;
{

  /*a->w=strlen(a->label)*(float)strwidth("a")/a->p->ppu+2.0*PNL_DIM_3;*/
    a->h = a->lh;
    a->w = a->lw;
}



/* dpg */
void
_addlabel (a, p)
    Actuator *a;
    Panel *p;
{
    /* make act size agree w/ label size */
    /*  old way
    a->h = a->lh;
    a->w = a->lw;
    */
    a->h = a->lh+2./p->ppu;
    a->w = a->lw+2./p->ppu;
    a->x = a->x-1./p->ppu;
    a->y = a->y-1./p->ppu;
    a->dirtycnt = 2;
}
