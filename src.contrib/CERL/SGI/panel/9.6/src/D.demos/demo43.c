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

Actuator *scroll;
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
  int i,j;

  p=pnl_mkpanel();
  p->label="scrolling";

  scroll=a=pnl_mkact(pnl_scroll);
  a->w=5-PNL_DIM_1;
  a->h=8-PNL_DIM_1;
  pnl_addact(a, p);

  y=0;
  for (i=0; i<4; i++) {
    x=0;
    for (j=0; j<10; j++) {
      a=pnl_mkact(pnl_slider);
      a->x=x; x+=a->w+PNL_DIM_1;
      a->y=y;
      pnl_addsubact(a, scroll);
    }
    y+=a->h+PNL_DIM_1;
  }

  pnl_fixact(scroll);

  return p;
}

