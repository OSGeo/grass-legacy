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
#include <math.h>
#include <gl.h>
#include <panel.h>

void _newvaldial(a,p,x,y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{    
  Dial *ad=(Dial *)a->data;
  float theta, dtheta;
  
  if (!a->active) return;
  
  theta = atan2(x-a->x-a->w/2.0, y-a->y-a->h/2.0);

  if (pnl_justdown) {
    ad->wsave = theta;
    if (!pnl_controlkey&&!ad->mode&PNL_DM_NOSNAP)
      a->val=PNL_WORLD_TO_VAL(theta, (a->minval+a->maxval)/2.0,
			      a->maxval, ad->winds*PI);
  }

  dtheta=theta-ad->wsave;
  if (dtheta> PI) dtheta-=2.0*PI;;
  if (dtheta<-PI) dtheta+=2.0*PI;

  if (pnl_controlkey)
    a->val+=ad->finefactor
      *PNL_WORLD_TO_VAL(dtheta, 0.0, a->maxval-a->minval, ad->winds*2.0*PI);
  else
    a->val+=PNL_WORLD_TO_VAL(dtheta, 0.0,
			     a->maxval-a->minval, ad->winds*2.0*PI);

  if (ad->mode&PNL_DM_WRAP) {
    if (a->val>a->maxval) a->val-=(a->maxval-a->minval);
    if (a->val<a->minval) a->val+=(a->maxval-a->minval);
  } else 
    a->val=RANGE(a->val, a->maxval, a->minval);

  ad->wsave=theta;
  pnl_setdirty(a);
}

void
_drawdial(a, p)
Actuator *a;
Panel *p;
{
  float thetadeg;
  Dial *ad=(Dial *)a->data;

  if (!a->dirtycnt) return;

  pushmatrix();
  
  translate(a->x,a->y,0.0);
  
  color(pnl_other_color);
  rectf(0.0,0.0,a->w,a->h);
  color(pnl_black_color);
  rect(0.0,0.0,a->w,a->h);
  color(pnl_normal_color);
  circf(a->w/2.0, a->h/2.0, MIN(a->w/2.0, a->h/2.0)-PNL_DIM_3);
  color(pnl_black_color);
  circ(a->w/2.0, a->h/2.0, MIN(a->w/2.0, a->h/2.0)-PNL_DIM_3);

  translate(a->w/2.0, a->h/2.0, 0.0);

  thetadeg=PNL_VAL_TO_WORLD(RANGE(a->val, a->minval, a->maxval),
			    a->minval, a->maxval,
			    ad->winds*2.0*PI)*180.0/PI;
  rot(-thetadeg+ad->winds*180.0+90.0, 'z');

  color(pnl_highlight_color);
  rectf(0.0,-PNL_DIM_4, MIN(a->w/2.0, a->h/2.0)-PNL_DIM_3, PNL_DIM_4);
  color(pnl_black_color);
  rect(0.0,-PNL_DIM_4, MIN(a->w/2.0, a->h/2.0)-PNL_DIM_3, PNL_DIM_4);
  
  popmatrix();
  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
_hitdial()
{
}

void
pnl_dial(a)
Actuator *a;
{
  Dial *ad;

  a->type=PNL_DIAL;

  a->data = (char *)pnl_alloc(sizeof(Dial));
  a->datasize = sizeof(Dial);
  ad=(Dial *)a->data;
  ad->wsave=0.0;
  ad->finefactor=PNL_FINE_CONTROL_FACTOR;
  ad->winds=PNL_DIAL_WINDS;

  a->labeltype=PNL_LABEL_BOTTOM;

  a->w=PNL_DIAL_EDGE;
  a->h=PNL_DIAL_EDGE;
  a->newvalfunc=_newvaldial;
  a->drawfunc=_drawdial;
}

