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

main() 
{
  foreground();
  noport();
  winopen("demo");
  defpanel();

  for (;;) {
    pnl_dopanel();
    swapbuffers();
  }
}

void updatevaldisplay(a)
Actuator *a;
{
  Actuator *target=(Actuator *)a->u;

  sprintf(target->label, "%f", a->val);
  pnl_fixact(target);
}

defpanel()
{
  Panel *p;
  Actuator *a, *b;
  Dial *dd;
  float x, y;
  
  p=pnl_mkpanel();
  p->label="two dials";
  
  b=pnl_mkact(pnl_label);
  b->label=(char *)pnl_alloc(20);
  b->x=x+1;
  b->y=y-1;
  pnl_addact(b, p);

  a=pnl_mkact(pnl_dial);
  a->label="normal";
  a->x=x;
  a->y=y;
  a->w=a->h=3;
  a->activefunc=updatevaldisplay;
  a->u=(char *)b;
  pnl_addact(a, p);
  
  x+=a->w+PNL_DIM_1;
  
  b=pnl_mkact(pnl_label);
  b->label=(char *)pnl_alloc(20);
  b->x=x+1;
  b->y=y-1;
  pnl_addact(b, p);

  a=pnl_mkact(pnl_dial);
  a->label="wrap";
  a->x=x;
  a->y=y;
  a->w=a->h=3;
  a->activefunc=updatevaldisplay;
  a->u=(char *)b;
  dd=(Dial *)a->data;
  dd->mode|=PNL_DM_WRAP;
/*dd->winds=1.0;		/* so it goes around exactly once */
  pnl_addact(a, p);
}

