/*
 *	This software is in the public domain, it may not be resold
 *	or relicensed.  Modified and enhanced versions of this software
 *	are likewise to be made freely available.  Sites using this
 *	software are requested to register with NASA at the address below.  
 *	Send modifications and requests for most recent version to:
 *
 *	Author:  David A. Tristram,  ATTN: Panel Library
 *		 M/S T045-1
 *		 Ames Research Center
 *		 National Aeronautics and Space Administration
 *		 Moffett Field, CA  94035-4000
 *
 *		 415-694-4404
 *		 dat@nas.nasa.gov
 */
#include <gl.h>
#include <device.h>
#include <panel.h>

void
_addsubsubmenu(sa, a)
     Actuator *sa, *a;
{
  if (!a->ca) {
    a->na++;
    sa->pa=a;
  } else (*a->ca->addsubfunc)(sa, a->ca);  /* a->ca is the submenu's menu */
}

void
_add_submenu(a)
     Actuator *a;
{
  Actuator *sa;
  extern void _addicon();

  _addicon(a);

  sa=pnl_mkact(pnl_menu);
  sa->label=a->label;
  pnl_addsubact(sa, a);

  a->al=sa;

  a->addsubfunc=_addsubsubmenu;  /* subsequent calls to pnl_addsubact will */
				 /* add sub acts to the menu, not the icon */
}

void
_fixsubmenu(a)
     Actuator *a;
{
  Icon *ad=(Icon *)a->data;
  extern void _fixicon();

  _fixicon(a);
  ad->xopen=a->x+ad->wstowed/3.0;
}

void
pnl_sub_menu(a)
Actuator *a;
{
  pnl_icon(a);

  a->type=PNL_SUB_MENU;
  a->fixfunc=_fixsubmenu;
  a->addfunc=_add_submenu;
}

