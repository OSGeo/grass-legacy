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

Actuator *menu, *submenu1, *submenu2, *submenu3,
  *amenuitem, *bmenuitem, *cmenuitem;
  
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

  p=pnl_mkpanel();
  p->label="menus";

  menu=a=pnl_mkact(pnl_icon_menu);
  a->label="a nested menu";
  pnl_addact(menu, p);

  submenu1=a=pnl_mkact(pnl_sub_menu);
  a->label="menu 1";
  pnl_addsubact(submenu1, menu);

  amenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="first choice";
  pnl_addsubact(a, submenu1);

  bmenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="a big problem";
  pnl_addsubact(a, submenu1);

  cmenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="the end";
  pnl_addsubact(a, submenu1);

  submenu2=a=pnl_mkact(pnl_sub_menu);
  a->label="another menu";
  pnl_addsubact(submenu2, menu);

  amenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="oneish";
  pnl_addsubact(a, submenu2);

  bmenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="twoish";
  pnl_addsubact(a, submenu2);

  cmenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="threeish";
  pnl_addsubact(a, submenu2);

  submenu3=a=pnl_mkact(pnl_sub_menu);
  a->label="thirdly";
  pnl_addsubact(submenu3, menu);

  amenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="1";
  pnl_addsubact(a, submenu3);

  bmenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="2";
  pnl_addsubact(a, submenu3);

  cmenuitem=a=pnl_mkact(pnl_menu_item);
  a->label="3";
  pnl_addsubact(a, submenu3);

  pnl_fixact(menu);

  return p;
}

