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

Actuator *icon, *menu, *aslider, *bslider, *cslider;
  
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

Panel 
*defpanel()
{
  Actuator *a;
  Panel *p;
  Coord x, y, d=1.0, dl=0.5;

  p=pnl_mkpanel();
  p->label="menus";

  icon=a=pnl_mkact(pnl_icon);
  pnl_addact(icon, p);

  menu=a=pnl_mkact(pnl_menu);
  a->label="a menu";
  pnl_addsubact(menu, icon);

  aslider=a=pnl_mkact(pnl_hslider);
  a->label="first choice";
  a->labeltype=PNL_LABEL_RIGHT;
  pnl_addsubact(a, menu);

  bslider=a=pnl_mkact(pnl_hslider);
  a->label="a big problem";
  a->labeltype=PNL_LABEL_RIGHT;
  pnl_addsubact(a, menu);

  cslider=a=pnl_mkact(pnl_hslider);
  a->label="the end";
  a->labeltype=PNL_LABEL_RIGHT;
  pnl_addsubact(a, menu);

  pnl_fixact(icon);

  return p;
}

