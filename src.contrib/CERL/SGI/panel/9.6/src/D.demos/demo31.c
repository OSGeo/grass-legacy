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

Actuator *icon, *menu, *amenuitem, *bmenuitem, *cmenuitem;
  
Panel *defpanel();

main() 
{
  foreground();
  noport();
  winopen("demo");
  
  doublebuffer();
  gconfig();
  
  defpanel();
  
  for (;;) {
    pnl_dopanel();
    swapbuffers();
  }
}

void
printfunc(a)
Actuator *a;
{
#if 0
  switch (pnl_funcmode) {
  case PNL_FCNM_NONE:
    printf("no function mode\n, actuator %s",
	   (a->label?a->label:"<no label>"));
    break;
  case PNL_FCNM_DOWN:
    printf("downfunc, actuator %s\n", (a->label?a->label:"<no label>"));
    break;
  case PNL_FCNM_ACTIVE:
    printf("activefunc, actuator %s\n", (a->label?a->label:"<no label>"));
    break;
  case PNL_FCNM_UP:
    printf("upfunc, actuator %s\n", (a->label?a->label:"<no label>"));
    break;
  }
#endif 0
}

Panel 
*defpanel()
{
  Actuator *a;
  Panel *p;
  Coord x, y, d=1.0, dl=0.5;

  p=pnl_mkpanel();
  p->label="menus";

  icon=a=pnl_mkact(pnl_icon);
  a->downfunc=a->activefunc=a->upfunc=printfunc;
  pnl_addact(icon, p);

  menu=a=pnl_mkact(pnl_menu);
  a->label="a menu";
  a->downfunc=a->activefunc=a->upfunc=printfunc;
  pnl_addsubact(menu, icon);

  amenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="first choice";
  a->downfunc=a->activefunc=a->upfunc=printfunc;
  pnl_addsubact(a, menu);

  bmenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="a big problem";
  a->downfunc=a->activefunc=a->upfunc=printfunc;
  pnl_addsubact(a, menu);

  cmenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="the end";
  a->downfunc=a->activefunc=a->upfunc=printfunc;
  pnl_addsubact(a, menu);

  pnl_fixact(icon);

  return p;
}

