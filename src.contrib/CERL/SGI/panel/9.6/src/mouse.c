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
#include <stdio.h>
#include <gl.h>
#include <panel.h>

void _newvalmouse(a,p,x,y)
Actuator *a;
Panel *p;
Coord x, y;
{
  Mouse *ad=(Mouse *)a->data;

  ad->x=pnl_mx;
  ad->y=pnl_my;

  if (a->active) a->val=1.0;
  else		 a->val=0.0;
}

void
_addmouse(a, p)
Actuator *a;
Panel *p;
{
  if (pnl_mouse_act) {
    (void) fprintf(stderr, "libpanel: warning, duplicate pnl_mouse actuator\n");
    return;
  }
  a->p=NULL;
  a->data=(char *)pnl_alloc(sizeof(Mouse));
  pnl_mouse_act=a;
}

void
pnl_mouse(a)
Actuator *a;
{
    a->type=PNL_MOUSE;

/*     a->automatic=TRUE; */
    a->visible=FALSE;
    a->newvalfunc=_newvalmouse;
    a->addfunc=_addmouse;
    a->drawfunc=NULL;
}

