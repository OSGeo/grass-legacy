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

Panel *panel;
Actuator *vf[3];
  
Panel *defpanel();

main() 
{
  foreground();
  noport();
  winopen("demo");
  
  doublebuffer();
  gconfig();
  
  panel=defpanel();
  
  for (;;) {
    pnl_dopanel();
    swapbuffers();
  }
}

void
  updaterotx1(a)
  Actuator *a;
{
  int i;
  Viewframe *ad;

  for (i=0;i<3;i++) {
    ad=(Viewframe *)vf[i]->data;
    editobj(ad->vobj);
    objreplace(PNL_VFT_ROTATE_X1);
    rot(a->val*360.0, 'x');
    closeobj();
    pnl_fixact(vf[i]);
    panel->dirtycnt=2;
  }
}

void
  updaterotz(a)
  Actuator *a;
{
  int i;
  Viewframe *ad;

  for (i=0;i<3;i++) {
    ad=(Viewframe *)vf[i]->data;
    editobj(ad->vobj);
    objreplace(PNL_VFT_ROTATE_Z);
    rot(a->val*360.0, 'z');
    closeobj();
    pnl_fixact(vf[i]);
    panel->dirtycnt=2;
  }
}

void
  updaterotx2(a)
  Actuator *a;
{
  int i;
  Viewframe *ad;

  for (i=0;i<3;i++) {
    ad=(Viewframe *)vf[i]->data;
    editobj(ad->vobj);
    objreplace(PNL_VFT_ROTATE_X2);
    rot(a->val*360.0, 'x');
    closeobj();
    pnl_fixact(vf[i]);
    panel->dirtycnt=2;
  }
}

void
  updatetran1(a)
  Actuator *a;
{
  int i;
  Viewframe *ad;

  for (i=0;i<3;i++) {
    ad=(Viewframe *)vf[i]->data;
    editobj(ad->vobj);
    objreplace(PNL_VFT_TRANSLATE1);
    translate(a->val, a->val, 0.0);
    closeobj();
    pnl_fixact(vf[i]);
    panel->dirtycnt=2;
  }
}

void
  updatetran2(a)
  Actuator *a;
{
  int i;
  Viewframe *ad;

  for (i=0;i<3;i++) {
    ad=(Viewframe *)vf[i]->data;
    editobj(ad->vobj);
    objreplace(PNL_VFT_TRANSLATE2);
    translate(a->val, a->val, 0.0);
    closeobj();
    pnl_fixact(vf[i]);
    panel->dirtycnt=2;
  }
}

void
  updatescale(a)
  Actuator *a;
{
  int i;
  Viewframe *ad;

  for (i=0;i<3;i++) {
    ad=(Viewframe *)vf[i]->data;
    ad->sx=ad->sy=a->val;
    pnl_fixact(vf[i]);
    panel->dirtycnt=2;
  }
}

Actuator
*defviewframe(p)
   Panel *p;
{
  Actuator *viewframe, *subviewframe, *a;
  Coord x=0, y=0;
  Coord d=1.0, dl=0.5;
  int i;

  viewframe=pnl_mkact(pnl_viewframe);
  viewframe->beveled=FALSE;
  pnl_addact(viewframe, p);

  a=pnl_mkact(pnl_slider);
  a->x=x; x+=a->w+PNL_DIM_1;
  a->beveled=FALSE;
  pnl_addsubact(a, viewframe);

  a=pnl_mkact(pnl_slider);
  a->x=x; x+=a->w+PNL_DIM_1;
  a->beveled=FALSE;
  pnl_addsubact(a, viewframe);

  {
    subviewframe=pnl_mkact(pnl_viewframe);
    subviewframe->label="sub viewframe";
    subviewframe->beveled=FALSE;
    subviewframe->x=x;
    pnl_addsubact(subviewframe, viewframe);
    
    a=pnl_mkact(pnl_slider);
    a->x=10;
    a->y=3;
    a->label="slider in viewframe";
    a->beveled=FALSE;
    pnl_addsubact(a, subviewframe);
    
    x+=subviewframe->w+PNL_DIM_1;
  }

  for (i=0;i<8;i++) {
    a=pnl_mkact(pnl_button);
    a->x=x;
    a->y=y; y+=a->h+PNL_DIM_1;
    a->beveled=FALSE;
    pnl_addsubact(a, viewframe);
  }

  x+=a->w+PNL_DIM_1;

  a=pnl_mkact(pnl_slider);
  a->x=x; x+=a->w+PNL_DIM_1;
  a->beveled=FALSE;
  pnl_addsubact(a, viewframe);

  return viewframe;
}

Panel 
*defpanel()
{
  Actuator *a;
  Panel *p;
  Coord x, y, d=1.0, dl=0.5;
  float sf;

  p=pnl_mkpanel();
  p->label="viewframes";

  a=pnl_mkact(pnl_slider);
  a->label="rotx1";
  a->x=x;
  a->activefunc=updaterotx1;
  pnl_addact(a, p);
  x+=a->w+PNL_DIM_1;

  a=pnl_mkact(pnl_slider);
  a->label="rotz";
  a->x=x;
  a->activefunc=updaterotz;
  pnl_addact(a, p);
  x+=a->w+PNL_DIM_1;

  a=pnl_mkact(pnl_slider);
  a->label="rotx2";
  a->x=x;
  a->activefunc=updaterotx2;
  pnl_addact(a, p);
  x+=a->w+PNL_DIM_1;

  a=pnl_mkact(pnl_slider);
  a->label="tran1";
  a->x=x;
  a->activefunc=updatetran1;
  pnl_addact(a, p);
  x+=a->w+PNL_DIM_1;

  a=pnl_mkact(pnl_slider);
  a->label="tran2";
  a->x=x;
  a->activefunc=updatetran2;
  pnl_addact(a, p);
  x+=a->w+PNL_DIM_1;

  a=pnl_mkact(pnl_slider);
  a->label="scale";
  a->x=x;
  a->val=1;
  a->minval=0;
  a->maxval=5;
  a->activefunc=updatescale;
  pnl_addact(a, p);
  x+=a->w+PNL_DIM_1;

  vf[0]=a=defviewframe(p);
  a->label="viewframe one";
  a->x=x;
  a->scalefactor=sf=1.0;
  pnl_fixact(a);
  x+=a->w+PNL_DIM_1;

  vf[1]=a=defviewframe(p);
  a->label="viewframe two";
  a->x=x;
  a->scalefactor=(sf*=0.66);
  pnl_fixact(a);
  x+=a->w+PNL_DIM_1;

  vf[2]=a=defviewframe(p);
  a->label="viewframe three";
  a->x=x;
  a->scalefactor=(sf*=0.66);
  pnl_fixact(a);
  x+=a->w+PNL_DIM_1;

  return p;
}

