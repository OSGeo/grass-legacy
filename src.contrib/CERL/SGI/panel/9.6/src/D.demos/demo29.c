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

Actuator *mslider, *freqslider, *ulbar, *valbar, *llbar;
  
Panel *defpanel();

main() 
{
  Panel *p;

  foreground();
  noport();
  winopen("demo");
  
  doublebuffer();
  gconfig();
  
  p=defpanel();
  
  for (;;) {
    pnl_dopanel();
    animatesliders();
    swapbuffers();
  }
}

animatesliders()
{
  static float sense=1;
  float dx=sense*freqslider->val*
    (ulbar->extval-llbar->extval)/(mslider->maxval-mslider->minval);

  if (freqslider->val==0.0) return;

  valbar->extval+=dx;

  if (valbar->extval<llbar->extval) {
    valbar->extval= -valbar->extval+2*llbar->extval;
    sense=1;
  } else if (valbar->extval>ulbar->extval) {
    valbar->extval= -valbar->extval+2*ulbar->extval;
    sense= -1;
  }

  valbar->extval=RANGE(valbar->extval, llbar->extval, ulbar->extval);
  pnl_fixact(mslider); 
}

float range, center;

void saverange(a)
Actuator *a;
{
  range=ulbar->extval-llbar->extval;
  center=ulbar->extval-range/2.0;
}

void setcenter(a)
Actuator *a;
{
  llbar->extval=ulbar->extval-range;
  llbar->extval=RANGE(llbar->extval, mslider->minval, ulbar->extval);
  valbar->extval=RANGE(valbar->extval, llbar->extval, ulbar->extval);
  saverange(a);
  pnl_fixact(mslider);
}

void setrange(a)
Actuator *a;
{
  ulbar->extval=2*center-llbar->extval;
  ulbar->extval=RANGE(ulbar->extval, llbar->extval, mslider->maxval);
  valbar->extval=RANGE(valbar->extval, llbar->extval, ulbar->extval);
  saverange(a);
  pnl_fixact(mslider);
}

Panel 
*defpanel()
{
  Actuator *a;
  Panel *p;
  Coord x, y, d=1.0, dl=0.5;

  p=pnl_mkpanel();
  p->label="modulation";

  mslider=a=pnl_mkact(pnl_multislider);
  PNL_ACCESS(Multislider, a, mode)=PNL_MSM_ORDERED;
  PNL_ACCESS(Multislider, a, n)=0;
  a->label="mslider";
  a->maxval=1;
  a->minval= -1;
  a->val=0;
  pnl_addact(a, p);

  ulbar=a=pnl_mkact(pnl_multislider_bar);
  a->extval= 1;
  a->label="< hi";
  a->labeltype=PNL_LABEL_RIGHT;
  a->downfunc=saverange;
  a->activefunc=setcenter;
  pnl_addsubact(a, mslider);

  valbar=a=pnl_mkact(pnl_multislider_bar);
  a->extval=0;
  pnl_addsubact(a, mslider);

  llbar=freqslider=a=pnl_mkact(pnl_hslider);
  a->extval= -1;
  a->maxval=(mslider->maxval-mslider->minval)/2.0;
  a->label="< lo";
  a->labeltype=PNL_LABEL_RIGHT;
  PNL_ACCESS(Slider, a, mode)|=PNL_SM_NOSNAP;
  a->downfunc=saverange;
  a->activefunc=setrange;
  pnl_addsubact(a, mslider);

  return p;
}

