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

Actuator *ms;
Panel
*defpanel();

Actuator *add_button, *delete_button,
  *free_slider, *ordered_slider, *constrained_slider;
  
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

#define MKACT(_a, _type,_label)	\
  a=(_a)=pnl_mkact(_type);		\
  (_a)->label=_label

#define ADDACT \
  a->x=x;			\
  a->y=(y-=a->h+dl+PNL_DIM_1);	\
  pnl_addact(a, p)

#define OVER	\
  x+=a->w+PNL_DIM_1;	\
  y+=a->h+dl+PNL_DIM_1

void
setaddmode(a)
Actuator *a;
{
  if (a->val==1.0) {
    PNL_ACCESS(Multislider, free_slider, mode) |= PNL_MSM_ADD;
    PNL_ACCESS(Multislider, ordered_slider, mode) |= PNL_MSM_ADD;
    PNL_ACCESS(Multislider, constrained_slider, mode) |= PNL_MSM_ADD;
    PNL_ACCESS(Multislider, free_slider, mode) &= ~PNL_MSM_DELETE;
    PNL_ACCESS(Multislider, ordered_slider, mode) &= ~PNL_MSM_DELETE;
    PNL_ACCESS(Multislider, constrained_slider, mode) &= ~PNL_MSM_DELETE;
  } else {
    PNL_ACCESS(Multislider, free_slider, mode) &= ~PNL_MSM_ADD;
    PNL_ACCESS(Multislider, ordered_slider, mode) &= ~PNL_MSM_ADD;
    PNL_ACCESS(Multislider, constrained_slider, mode) &= ~PNL_MSM_ADD;
  }
}

void
setdeletemode(a)
Actuator *a;
{
  if (a->val==1.0) {
    PNL_ACCESS(Multislider, free_slider, mode) |= PNL_MSM_DELETE;
    PNL_ACCESS(Multislider, ordered_slider, mode) |= PNL_MSM_DELETE;
    PNL_ACCESS(Multislider, constrained_slider, mode) |= PNL_MSM_DELETE;
    PNL_ACCESS(Multislider, free_slider, mode) &= ~PNL_MSM_ADD;
    PNL_ACCESS(Multislider, ordered_slider, mode) &= ~PNL_MSM_ADD;
    PNL_ACCESS(Multislider, constrained_slider, mode) &= ~PNL_MSM_ADD;
  } else {
    PNL_ACCESS(Multislider, free_slider, mode) &= ~PNL_MSM_DELETE;
    PNL_ACCESS(Multislider, ordered_slider, mode) &= ~PNL_MSM_DELETE;
    PNL_ACCESS(Multislider, constrained_slider, mode) &= ~PNL_MSM_DELETE;
  }
}

void
unsetmodes()
{
  add_button->val=delete_button->val=0.0;
  pnl_fixact(add_button);
  pnl_fixact(delete_button);
}

void
unsetmodesifoff(a)
     Actuator *a;
{
  if (a->val==0.0) unsetmodes(a);
}

void
_newvaltoggleradiobutton(a,p,x,y)
Actuator *a;
Panel *p;
Coord x,y;
{
  float saveval=a->val;
  void _newvaltogglebutton();
  void _newvalradiobutton();


  _newvalradiobutton(a,p,x,y);
  if (pnl_justdown) a->val=(saveval?0.0:1.0);
}


Panel
*defpanel()
{
  Actuator *a;
  Panel *p;
  Coord x, y, d=1.0, dl=0.5;

  p=pnl_mkpanel();
  p->label="multislider";

  MKACT(a, pnl_multislider, "free");
  a->selectable=0;
  free_slider=a;
  PNL_ACCESS(Multislider, a, mode)=PNL_MSM_FREE;
  ADDACT;

  OVER;
  x+=1;
  
  MKACT(a, pnl_multislider, "ordered");
  ordered_slider=a;
  PNL_ACCESS(Multislider, a, mode)=PNL_MSM_ORDERED;
  ADDACT;

  OVER;
  x+=1;

  MKACT(a, pnl_multislider, "constrained");
  constrained_slider=a;
  PNL_ACCESS(Multislider, a, mode)=PNL_MSM_CONSTRAINED;
  ADDACT;

  x=2;

  MKACT(a, pnl_radio_button, "add");
  add_button=a;
  a->newvalfunc=_newvaltoggleradiobutton;
  a->downfunc=setaddmode;
  ADDACT;

  dl=0;

  MKACT(a, pnl_radio_button, "delete");
  delete_button=a;
  a->newvalfunc=_newvaltoggleradiobutton;
  a->downfunc=setdeletemode;
  ADDACT;

  return p;
}

