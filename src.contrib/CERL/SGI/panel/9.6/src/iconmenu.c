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
_addsubiconmenu(sa, a)
     Actuator *sa, *a;
{
  if (!a->ca) {
    a->na++;
    sa->pa=a;
  } else (*a->ca->addsubfunc)(sa, a->ca);	/* a->ca is the icon's menu */
}

void
_addiconmenu(a)
     Actuator *a;
{
  Actuator *sa;
  extern void _addicon();

  _addicon(a);

  sa=pnl_mkact(pnl_menu);
  sa->label=a->label;
  sa->visible=FALSE;
  pnl_addsubact(sa, a);

  a->ca=sa;
  a->al=sa;
}

void
pnl_icon_menu(a)
Actuator *a;
{
  pnl_icon(a);

  a->type=PNL_ICON_MENU;
  a->addfunc=_addiconmenu;
  a->addsubfunc=_addsubiconmenu;
}

