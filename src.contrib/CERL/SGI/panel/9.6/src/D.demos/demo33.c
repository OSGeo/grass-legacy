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
  Actuator *a, *icon, *menu, *amenuitem, *bmenuitem, *cmenuitem;
  Panel *p;
  Coord x, y, d=1.0, dl=0.5;

  p=pnl_mkpanel();
  p->label="icons";

  icon=a=pnl_mkact(pnl_icon);
  a->x=x;
  pnl_addact(icon, p);

  menu=a=pnl_mkact(pnl_menu);
  a->label="menu 1";
  pnl_addsubact(menu, icon);

  amenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="first choice";
  pnl_addsubact(a, menu);

  bmenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="a big problem";
  pnl_addsubact(a, menu);

  cmenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="the end";
  pnl_addsubact(a, menu);

  pnl_fixact(icon);
  x+=icon->w+PNL_DIM_1;

  icon=a=pnl_mkact(pnl_icon);
  a->x=x;
  pnl_addact(icon, p);

  menu=a=pnl_mkact(pnl_menu);
  a->label="another menu";
  pnl_addsubact(menu, icon);

  amenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="oneish";
  pnl_addsubact(a, menu);

  bmenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="twoish";
  pnl_addsubact(a, menu);

  cmenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="threeish";
  pnl_addsubact(a, menu);

  pnl_fixact(icon);
  x+=icon->w+PNL_DIM_1;

  icon=a=pnl_mkact(pnl_icon);
  a->x=x;
  pnl_addact(icon, p);

  menu=a=pnl_mkact(pnl_menu);
  a->label="thirdly";
  pnl_addsubact(menu, icon);

  amenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="1";
  pnl_addsubact(a, menu);

  bmenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="2";
  pnl_addsubact(a, menu);

  cmenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="3";
  pnl_addsubact(a, menu);

  pnl_fixact(icon);
  x+=icon->w+PNL_DIM_1;

  return p;
}

