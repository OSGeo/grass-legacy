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
#include <panel.h>

void setsliderval(a, x, dist)
     Actuator *a;
     Coord x;
     float dist;
{
  Slider *ad=(Slider *)a->data;
  float tmp;

  tmp=PNL_VAL_TO_WORLD(a->val, a->minval, a->maxval, dist);
  if (pnl_justdown) {
    if (!pnl_controlkey&&!(ad->mode&PNL_SM_NOSNAP))
      a->val=PNL_WORLD_TO_VAL(x, a->minval, a->maxval, dist);
    ad->wsave=x;
  }
  
  a->val+=
    (pnl_controlkey?ad->finefactor
      *(x-ad->wsave)/dist*(a->maxval-a->minval)
      :(x-ad->wsave)/dist*(a->maxval-a->minval));
  a->val=RANGE(a->val, a->maxval, a->minval);

  ad->wsave=x;
  ad->valsave=a->val;
}

void _newvalvslider(a,p,x,y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{    
  Slider *ad=(Slider *)a->data;
  float tmp;
  
  if (a->active) setsliderval(a, y-a->y, a->h);
  pnl_setdirty(a);
}

void _newvalhslider(a,p,x,y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{    
  Slider *ad=(Slider *)a->data;
  
  if (a->active) setsliderval(a, x-a->x, a->w);
  pnl_setdirty(a);
}

void _newvaldvslider(a,p,x,y)
Actuator *a;
Panel *p;
Coord x, y;
{    
  Slider *ad=(Slider *)a->data;

  pnl_setdirty(a);

  if (pnl_justdown) {
    if (y-a->y<a->h-(a->h/12.0)) ad->mode = PNL_SM_NORMAL;
    else {
      ad->wsave=y;
      ad->valsave=a->val;
      if (x-a->x<(a->w/2.0)) ad->mode = PNL_SM_DIFFERENTIAL;
      else 		     ad->mode = PNL_SM_FINE_CONTROL;
    }
  }
  if (pnl_justup) ad->mode=PNL_SM_NORMAL;
  if (!a->active) return;

  switch (ad->mode) {
  case PNL_SM_NORMAL:
    setsliderval(a, y-a->y, a->h-(a->h/12.0));
    break;
  case PNL_SM_DIFFERENTIAL: /* use PNL_WORLD_TO_VAL */
    a->val+=ad->finefactor*(y-ad->wsave)/a->h*(a->maxval-a->minval);
    break;
  case PNL_SM_FINE_CONTROL:
    a->val=ad->valsave
      +ad->differentialfactor*(y-ad->wsave)/a->h*(a->maxval-a->minval);
  default:
    break;
  }

  a->val=RANGE(a->val, a->maxval, a->minval);
}

void
  _drawvslider(a, p)
Actuator *a;
Panel *p;
{
  Coord y;
  Slider *ad=(Slider *)a->data;
  
  if (!a->dirtycnt) return;
  
  pushmatrix();
  
  translate(a->x,a->y,0.0);
  
#if 0	/* this shit don't work in colormap mode so good */
#ifdef PNL_ZBUFFER  
  zbuffer(FALSE);
  zdraw(TRUE);
  color(WHITE);
  rectf(0.0,0.0,a->w,a->h);
  zdraw(FALSE);
  zbuffer(TRUE);
#endif PNL_ZBUFFER 
#endif
  
  PNL_ZTRANSLATE;
  color(pnl_normal_color);
  rectf(0.0,0.0,a->w,a->h);

#if 0
  PNL_ZTRANSLATE;
  color(pnl_black_color);
  rect(0.0,0.0,a->w,a->h);
#endif
  
  y=(RANGE(a->val, a->minval, a->maxval)-a->minval)
    /(a->maxval-a->minval)*a->h;
  
  PNL_ZTRANSLATE;
  color(pnl_highlight_color);
  rectf(0.0,MAX(0.0,y-ad->bh/2.0),
	a->w,MIN(a->h,y+ad->bh/2.0));
  PNL_ZTRANSLATE;
  color(pnl_black_color);
  rect(0.0,MAX(0.0,y-ad->bh/2.0),
       a->w,MIN(a->h,y+ad->bh/2.0));
  
  popmatrix();
  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
_drawhslider(a, p)
Actuator *a;
Panel *p;
{
  Coord x;
  Slider *ad=(Slider *)a->data;

  if (!a->dirtycnt) return;

    pushmatrix();

    translate(a->x,a->y,0.0);

    color(pnl_normal_color);
    rectf(0.0,0.0,a->w,a->h);
    color(pnl_black_color);
    rect(0.0,0.0,a->w,a->h);

    x=(RANGE(a->val, a->minval, a->maxval)-a->minval)
      /(a->maxval-a->minval)*a->w;

    color(pnl_highlight_color);
    rectf(MAX(0.0,x-ad->bh/2.0),0.0,
	  MIN(a->w,x+ad->bh/2.0),a->h);
    color(pnl_black_color);
    rect(MAX(0.0,x-ad->bh/2.0),0.0,
	 MIN(a->w,x+ad->bh/2.0),a->h);

    popmatrix();
    if (a->beveled) pnl_drawbevel(a, p);
    if (a->label) pnl_drawlabel(a, p);
}

Coord diamond[][2] = {
   { 0.0, -1.0 },
   { 1.0,  0.0 },
   { 0.0,  1.0 },
   {-1.0,  0.0 }
 };

_drawfineicon(a, selected)
Actuator *a;
Boolean selected;
{
  pushmatrix();
  if (selected) {
    color(pnl_highlight_color);
    rectf(-(a->w/4.0),-(a->h/24.0),
	   (a->w/4.0), (a->h/24.0));
    color(pnl_normal_color);
    scale((a->w/4.0)-PNL_DIM_3, (a->h/24.0)-PNL_DIM_3, 0.0);
    polf2(4, diamond);
    color(pnl_black_color);
    poly2(4, diamond);
  } else {
    scale((a->w/4.0)-PNL_DIM_3, (a->h/24.0)-PNL_DIM_3, 0.0);
    color(pnl_other_color);
    polf2(4, diamond);
    color(pnl_black_color);
    poly2(4, diamond);
  }
  popmatrix();
}

_drawdifficon(a, selected)
Actuator *a;
Boolean selected;
{
  pushmatrix();
  if (selected) {
    color(pnl_highlight_color);
    rectf(-(a->w/4.0),-(a->h/24.0),
	   (a->w/4.0), (a->h/24.0));
    color(pnl_normal_color);
    scale((a->w/4.0)-PNL_DIM_3, (a->h/24.0)-PNL_DIM_3, 0.0);
    poly2(4, diamond);
  } else {
    color(pnl_black_color);
    scale((a->w/4.0)-PNL_DIM_3, (a->h/24.0)-PNL_DIM_3, 0.0);
    poly2(4, diamond);
  }
  popmatrix();
}

void
_drawdvslider(a, p)
Actuator *a;
Panel *p;
{
  Slider *ad=(Slider *)a->data;
  Coord y;

  if (!a->dirtycnt) return;

  pushmatrix();
  
  translate(a->x,a->y,0.0);
  
  color(pnl_normal_color);
  rectf(0.0,0.0,a->w,a->h);
  
  pushmatrix();
  translate((a->w/4.0),a->h-(a->h/24.0),0.0);
  switch (ad->mode) {
  case PNL_SM_NORMAL:
    _drawdifficon(a, FALSE);
    translate((a->w/2.0), 0.0, 0.0);
    _drawfineicon(a, FALSE);
    break;
  case PNL_SM_FINE_CONTROL:
    _drawdifficon(a, FALSE);
    translate((a->w/2.0), 0.0, 0.0);
    _drawfineicon(a, TRUE);
    break;
  case PNL_SM_DIFFERENTIAL:
    _drawdifficon(a, TRUE);
    translate((a->w/2.0), 0.0, 0.0);
    _drawfineicon(a, FALSE);
    break;
  }
  popmatrix();
  
  color(pnl_black_color);
  rect(0.0,0.0,a->w,a->h);
  move2(0.0,a->h-(a->h/12.0));
  rdr2(a->w,0.0);
  move2((a->w/2.0),a->h-(a->h/12.0));
  rdr2(0.0,(a->h/12.0));
  
  y=(RANGE(a->val, a->minval, a->maxval)-a->minval)
    /(a->maxval-a->minval)*(a->h-(a->h/12.0));

  color(pnl_highlight_color);
  rectf(0.0,MAX(0.0,y-ad->bh/2.0),
	a->w,MIN(a->h-(a->h/12.0),y+ad->bh/2.0));
  color(pnl_black_color);
  rect(0.0,MAX(0.0,y-ad->bh/2.0),
       a->w,MIN(a->h-(a->h/12.0),y+ad->bh/2.0));
  
  popmatrix();
  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
_drawfilledhslider(a, p)
Actuator *a;
Panel *p;
{
Coord x;

  if (!a->dirtycnt) return;

    pushmatrix();

    translate(a->x,a->y,0.0);

    color(pnl_normal_color);
    rectf(0.0,0.0,a->w,a->h);
    color(pnl_black_color);
    rect(0.0,0.0,a->w,a->h);

    x=(RANGE(a->val, a->minval, a->maxval)-a->minval)
      /(a->maxval-a->minval)*a->w;

    color(pnl_highlight_color);
    rectf(0.0,0.0,x,a->h);
    color(pnl_black_color);
    rect(0.0,0.0,x,a->h);

    popmatrix();
    if (a->beveled) pnl_drawbevel(a, p);
    if (a->label) pnl_drawlabel(a, p);
}

void
_drawfilledvslider(a, p)
Actuator *a;
Panel *p;
{
Coord y;

  if (!a->dirtycnt) return;

    pushmatrix();

    translate(a->x,a->y,0.0);

    color(pnl_normal_color);
    rectf(0.0,0.0,a->w,a->h);
    color(pnl_black_color);
    rect(0.0,0.0,a->w,a->h);

    y=(RANGE(a->val, a->minval, a->maxval)-a->minval)
      /(a->maxval-a->minval)*a->h;

    color(pnl_highlight_color);
    rectf(0.0,0.0,a->w,y);
    color(pnl_black_color);
    rect(0.0,0.0,a->w,y);

    popmatrix();
    if (a->beveled) pnl_drawbevel(a, p);
    if (a->label) pnl_drawlabel(a, p);
}

void
_drawvshadowslider(a, p)
Actuator *a;
Panel *p;
{
Coord y;
    float p1x = 0.0;
    float p1y = 0.0;
    float p2x = 0.0;
    float p2y = a->h;
    float p3x = a->w;
    float p3y = a->h;
    float p4x = a->w;
    float p4y = 0.0;

    float dx = p3x/10.0;

    float p5x = p1x + dx;
    float p5y = p1y + dx;
    float p6x = p2x + dx;
    float p6y = p2y - dx;
    float p7x = p3x - dx;
    float p7y = p3y - dx;
    float p8x = p4x - dx;
    float p8y = p4y + dx;

    float half_width = PNL_SLIDER_BAR_HEIGHT/2.0;

    if (!a->dirtycnt) return;
 
    pushmatrix();
    translate(a->x,a->y,0.0);

    color(pnl_highlight_color);
    pmv2(p1x,p1y);
    pdr2(p2x,p2y);
    pdr2(p6x,p6y);
    pdr2(p5x,p5y);
    pclos();

    pmv2(p2x,p2y);
    pdr2(p3x,p3y);
    pdr2(p7x,p7y);
    pdr2(p6x,p6y);
    pclos();

    color(pnl_normal_color);
    pmv2(p3x,p3y);
    pdr2(p4x,p4y);
    pdr2(p8x,p8y);
    pdr2(p7x,p7y);
    pclos();

    pmv2(p4x,p4y);
    pdr2(p1x,p1y);
    pdr2(p5x,p5y);
    pdr2(p8x,p8y);
    pclos();

    color(pnl_other_color);

    rectf(p5x,p5y,p7x,p7y);

    y=(RANGE(a->val, a->minval, a->maxval)-a->minval)
      /(a->maxval-a->minval)*a->h;

    color(pnl_highlight_color);
    p1y = MAX(0.0,y-half_width-dx);
    p1x = dx;
    p2y = MAX(0.0,y-half_width-dx);
    p2x = a->w-dx;
    p3y = MAX(0.0,y-half_width);
    p3x = a->w;
    p4y = MAX(0.0,y-half_width);
    p4x = 0.0;

    pmv2(p1x,p1y);
    pdr2(p2x,p2y);
    pdr2(p3x,p3y);
    pdr2(p4x,p4y);
    pclos();

    color(pnl_background_color);
    rectf(0.0,MAX(0.0,y-half_width),
	  a->w,MIN(a->h,y+half_width));

    color(pnl_normal_color);
    move2(0.0,MAX(0.0,y-half_width));
    draw2(0.0,MIN(a->h,y+half_width));

    color(pnl_highlight_color);
    move2(a->w,MAX(0.0,y-half_width));
    draw2(a->w,MIN(a->h,y+half_width));

    color(pnl_normal_color);
    p1y = MIN(a->h,y+half_width+dx);
    p1x = dx;
    p2y = MIN(a->h,y+half_width+dx);
    p2x = a->w-dx;
    p3y = MIN(a->h,y+half_width);
    p3x = a->w;
    p4y = MIN(a->h,y+half_width);
    p4x = 0.0;

    pmv2(p1x,p1y);
    pdr2(p2x,p2y);
    pdr2(p3x,p3y);
    pdr2(p4x,p4y);
    pclos();

    popmatrix();
    if (a->beveled) pnl_drawbevel(a, p);
    if (a->label) pnl_drawlabel(a, p);
}

void
_drawhshadowslider(a, p)
Actuator *a;
Panel *p;
{
Coord x;
    float p1x = 0.0;
    float p1y = 0.0;
    float p2x = 0.0;
    float p2y = a->h;
    float p3x = a->w;
    float p3y = a->h;
    float p4x = a->w;
    float p4y = 0.0;

    float dy = p3y/10.0;

    float p5x = p1x + dy;
    float p5y = p1y + dy;
    float p6x = p2x + dy;
    float p6y = p2y - dy;
    float p7x = p3x - dy;
    float p7y = p3y - dy;
    float p8x = p4x - dy;
    float p8y = p4y + dy;

    float half_width = PNL_SLIDER_BAR_HEIGHT/2.0;

    pushmatrix();
    translate(a->x,a->y,0.0);

    color(pnl_highlight_color);
    pmv2(p1x,p1y);
    pdr2(p2x,p2y);
    pdr2(p6x,p6y);
    pdr2(p5x,p5y);
    pclos();

    pmv2(p2x,p2y);
    pdr2(p3x,p3y);
    pdr2(p7x,p7y);
    pdr2(p6x,p6y);
    pclos();

    color(pnl_normal_color);
    pmv2(p3x,p3y);
    pdr2(p4x,p4y);
    pdr2(p8x,p8y);
    pdr2(p7x,p7y);
    pclos();

    pmv2(p4x,p4y);
    pdr2(p1x,p1y);
    pdr2(p5x,p5y);
    pdr2(p8x,p8y);
    pclos();

    color(pnl_other_color);

    rectf(p5x,p5y,p7x,p7y);

    x=(RANGE(a->val, a->minval, a->maxval)-a->minval)
      /(a->maxval-a->minval)*a->w;

    color(pnl_normal_color);
    p1x = MAX(0.0,x-half_width-dy);
    p1y = dy;
    p2x = MAX(0.0,x-half_width-dy);
    p2y = a->h-dy;
    p3x = MAX(0.0,x-half_width);
    p3y = a->h;
    p4x = MAX(0.0,x-half_width);
    p4y = 0.0;

    pmv2(p1x,p1y);
    pdr2(p2x,p2y);
    pdr2(p3x,p3y);
    pdr2(p4x,p4y);
    pclos();

    color(pnl_background_color);
    rectf(MAX(0.0,x-half_width),0.0,
	  MIN(a->w,x+half_width),a->h);

    color(pnl_normal_color);
    move2(MAX(0.0,x-half_width),a->h);
    draw2(MIN(a->w,x+half_width),a->h);

    color(pnl_highlight_color);
    move2(MAX(0.0,x-half_width),0.0);
    draw2(MIN(a->w,x+half_width),0.0);

    color(pnl_highlight_color);

    p1x = MIN(a->w,x+half_width+dy);
    p1y = dy;
    p2x = MIN(a->w,x+half_width+dy);
    p2y = a->h-dy;
    p3x = MIN(a->w,x+half_width);
    p3y = a->h;
    p4x = MIN(a->w,x+half_width);
    p4y = 0.0;

    pmv2(p1x,p1y);
    pdr2(p2x,p2y);
    pdr2(p3x,p3y);
    pdr2(p4x,p4y);
    pclos();

    popmatrix();
    if (a->beveled) pnl_drawbevel(a, p);
    if (a->label) pnl_drawlabel(a, p);
}

void
pnl_slider(a)
Actuator *a;
{
  Slider *ad;

  a->type=PNL_SLIDER;

  a->data = (char *)pnl_alloc(sizeof(Slider));
  a->datasize = sizeof(Slider);
  ad=(Slider *)a->data;
  ad->wsave=0.0;
  ad->mode=PNL_SM_NORMAL;
  ad->bh=PNL_SLIDER_BAR_HEIGHT;
  ad->finefactor=PNL_FINE_CONTROL_FACTOR;
  ad->differentialfactor=PNL_DIFFERENTIAL_FACTOR;

  a->labeltype=PNL_LABEL_BOTTOM;

  a->w=PNL_SLIDER_WIDTH;
  a->h=PNL_SLIDER_HEIGHT;
  a->newvalfunc=_newvalvslider;
  a->drawfunc=_drawvslider;
}

void
pnl_vslider(a)
Actuator *a;
{
  pnl_slider(a);
  a->type=PNL_VSLIDER;

  a->w=PNL_SLIDER_WIDTH;
  a->h=PNL_SLIDER_HEIGHT;
  a->newvalfunc=_newvalvslider;
  a->drawfunc=_drawvslider;
}

void
pnl_hslider(a)
Actuator *a;
{
  pnl_slider(a);
  a->type=PNL_HSLIDER;

  a->w=PNL_SLIDER_HEIGHT;
  a->h=PNL_SLIDER_WIDTH;
  a->newvalfunc=_newvalhslider;
  a->drawfunc=_drawhslider;
}

void
pnl_dvslider(a)
Actuator *a;
{
  pnl_slider(a);
  a->type=PNL_DVSLIDER;
  
  a->w=PNL_SLIDER_WIDTH;
  a->h=PNL_SLIDER_HEIGHT;
  a->newvalfunc=_newvaldvslider;
  a->drawfunc=_drawdvslider;
}

void
pnl_filled_vslider(a)
Actuator *a;
{
  pnl_slider(a);
  a->type=PNL_FILLED_VSLIDER;
  
  a->w=PNL_SLIDER_WIDTH;
  a->h=PNL_SLIDER_HEIGHT;
  a->newvalfunc=_newvalvslider;
  a->drawfunc=_drawfilledvslider;
}

void
pnl_filled_slider(a)
Actuator *a;
{
  pnl_filled_vslider(a);
  a->type=PNL_FILLED_SLIDER;
}

void
pnl_filled_hslider(a)
Actuator *a;
{
  pnl_slider(a);
  a->type=PNL_FILLED_HSLIDER;

  a->w=PNL_SLIDER_HEIGHT;
  a->h=PNL_SLIDER_WIDTH;
  a->newvalfunc=_newvalhslider;
  a->drawfunc=_drawfilledhslider;
}
