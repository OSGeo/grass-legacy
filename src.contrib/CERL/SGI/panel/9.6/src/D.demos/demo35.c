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

Actuator *menu, *amenuitem, *bmenuitem, *cmenuitem;
  
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

Actuator
*defframe(p)
   Panel *p;
{
  Actuator *frame, *a;
  Coord x, y, d=1.0, dl=0.5;

  frame=pnl_mkact(pnl_frame);
  pnl_addact(frame, p);

  a=pnl_mkact(pnl_slider);
  a->x=x; x+=a->w+PNL_DIM_1;
  pnl_addsubact(a, frame);

  a=pnl_mkact(pnl_slider);
  a->x=x; x+=a->w+PNL_DIM_1;
  pnl_addsubact(a, frame);

  a=pnl_mkact(pnl_slider);
  a->x=x; x+=a->w+PNL_DIM_1;
  pnl_addsubact(a, frame);

  return frame;
}

Panel 
*defpanel()
{
  Actuator *a;
  Panel *p;
  Coord x, y, d=1.0, dl=0.5;
  float sf;

  p=pnl_mkpanel();
  p->label="frames";

  a=defframe(p);
  a->label="frame one";
  a->x=x;
  a->scalefactor=sf=1.0;
  pnl_fixact(a);
  x+=a->w+PNL_DIM_1;

  a=defframe(p);
  a->label="frame two";
  a->x=x;
  a->scalefactor=(sf*=0.66);
  pnl_fixact(a);
  x+=a->w+PNL_DIM_1;

  a=defframe(p);
  a->label="frame three";
  a->x=x;
  a->scalefactor=(sf*=0.66);
  pnl_fixact(a);
  x+=a->w+PNL_DIM_1;

  return p;
}

