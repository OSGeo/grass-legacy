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

#define NORMAL_MODE		0
#define DIFFERENTIAL_MODE	1
#define ABSOLUTE_MODE		2

#define DIFFERENTIAL_FACTOR	0.01
#define ABSOLUTE_FACTOR		0.05
#define FINE_FACTOR		0.05

void _newvalslideroid(a,p,x,y)
Actuator *a;
Panel *p;
Coord x, y;
{    
  Slideroid *ad=(Slideroid *)a->data;

  pnl_setdirty(a);

  if (pnl_justdown) {
    if (   y-a->y>2.0*a->h/3.0
	&& x-a->x<a->w/6.0) ad->finemode= ~ad->finemode;
    else if (   y-a->y>2.0*a->h/3.0
	     && x-a->x>5.0*a->w/6.0) {
      ad->resetmode=TRUE;
      if (a->val==a->initval
	  && ad->resettarget) *ad->resettarget=ad->resetval;
      else a->val=a->initval;
    }
    else if (y-a->y<a->h/3.0) {
      ad->wsave=y;
      ad->valsave=a->val;
      if (x-a->x<a->w/2.0) ad->mode = DIFFERENTIAL_MODE;
      else 		   ad->mode = ABSOLUTE_MODE;
    } else  ad->mode = NORMAL_MODE;
  }
  if (pnl_justup) {
    ad->mode=NORMAL_MODE;
    ad->resetmode=FALSE;
  }
  if (!a->active) return;

  switch (ad->mode) {
  case NORMAL_MODE:
    break;
  case DIFFERENTIAL_MODE:
    a->val+=DIFFERENTIAL_FACTOR
      *(ad->finemode?FINE_FACTOR:1.0)
	*(y-ad->wsave)
	  /a->h
	    *(a->maxval-a->minval);
    break;
  case ABSOLUTE_MODE:
    a->val=ad->valsave
      +ABSOLUTE_FACTOR
	*(ad->finemode?FINE_FACTOR:1.0)
	  *(y-ad->wsave)/a->h
	    *(a->maxval-a->minval);
  default:
    break;
  }

  a->val=RANGE(a->val, a->maxval, a->minval);
}

static Coord diamond[][2] = {
   { 0.0, -1.0 },
   { 1.0,  0.0 },
   { 0.0,  1.0 },
   {-1.0,  0.0 }
 };

static _drawabsicon(a, selected)
Actuator *a;
Boolean selected;
{
  pushmatrix();
  if (selected) {
    color(pnl_highlight_color);
    rectf(-a->w/4.0,-a->h/6.0,
	   a->w/4.0, a->h/6.0);
    color(pnl_normal_color);
    scale(a->w/4.0-PNL_DIM_3, a->h/6.0-PNL_DIM_3, 0.0);
    polf2(4, diamond);
    color(pnl_black_color);
    poly2(4, diamond);
  } else {
    scale(a->w/4.0-PNL_DIM_3, a->h/6.0-PNL_DIM_3, 0.0);
    color(pnl_other_color);
    polf2(4, diamond);
    color(pnl_black_color);
    poly2(4, diamond);
  }
  popmatrix();
}

static _drawdifficon(a, selected)
Actuator *a;
Boolean selected;
{
  pushmatrix();
  if (selected) {
    color(pnl_highlight_color);
    rectf(-a->w/4.0,-a->h/6.0,
	   a->w/4.0, a->h/6.0);
    color(pnl_normal_color);
    scale(a->w/4.0-PNL_DIM_3, a->h/6.0-PNL_DIM_3, 0.0);
    poly2(4, diamond);
  } else {
    color(pnl_black_color);
    scale(a->w/4.0-PNL_DIM_3, a->h/6.0-PNL_DIM_3, 0.0);
    poly2(4, diamond);
  }
  popmatrix();
}

void
_drawslideroid(a, p)
Actuator *a;
Panel *p;
{
  Slideroid *ad=(Slideroid *)a->data;
  Coord cx, cy;
  char s[7], stmp[16];

  if (!a->dirtycnt) return;

  pushmatrix();
  
  translate(a->x,a->y,0.0);
  
  color(pnl_normal_color);
  rectf(0.0,0.0,a->w,a->h);
  color(pnl_black_color);
  rect(0.0,0.0,a->w,a->h);
  
  move2(0.0,a->h/3.0);
  rdr2(a->w,0.0);
  move2(0.0,2.0*a->h/3.0);
  rdr2(a->w,0.0);
  move2(a->w/2.0,0.0);
  rdr2(0.0,a->h/3.0);
  move2(a->w/6.0,2.0*a->h/3.0);
  rdr2(0.0,a->h/3.0);
  move2(5.0*a->w/6.0,2.0*a->h/3.0);
  rdr2(0.0,a->h/3.0);

  cx= -(float)strwidth("F")/p->ppu/2.0;
  cy= -(float)(getheight("E")-getdescender())/p->ppu/2.0;
  if (ad->finemode) {
    color(pnl_highlight_color);
    rectf(0.0,2.0*a->h/3.0,a->w/6.0,a->h);
    color(pnl_white_color);
    cmov2(a->w/12.0+cx, 5.0*a->h/6.0+cy);
    charstr("F");
  } else {
    color(pnl_label_color);
    cmov2(a->w/12.0+cx, 5.0*a->h/6.0+cy);
    charstr("F");
  }

  if (ad->resetmode) {
    color(pnl_highlight_color);
    rectf(5.0*a->w/6.0,2.0*a->h/3.0,a->w,a->h);
    color(pnl_white_color);
    cmov2(11.0*a->w/12.0+cx, 5.0*a->h/6.0+cy);
    charstr("R");
  } else {
    color(pnl_label_color);
    cmov2(11.0*a->w/12.0+cx, 5.0*a->h/6.0+cy);
    charstr("R");
  }

  (void) sprintf(stmp, "%-+#10.3E", a->val);
  (void) strncpy(s, stmp, 6);
  s[6]='\0';

  color(pnl_label_color);
  cx= -(float)strwidth(s)/p->ppu/2.0;
  cmov2(a->w/2.0+cx, 3.0*a->h/6.0+cy);
  charstr(s);

  (void) strncpy(s, stmp+6, 4);
  s[4]='\0';

  cx= -(float)strwidth(s)/p->ppu/2.0;
  cmov2(a->w/2.0+cx, 5.0*a->h/6.0+cy);
  charstr(s);

  pushmatrix();
  translate(a->w/4.0,a->h/6.0,0.0);
  switch (ad->mode) {
  case NORMAL_MODE:
    _drawdifficon(a, FALSE);
    translate(a->w/2.0, 0.0, 0.0);
    _drawabsicon(a, FALSE);
    break;
  case ABSOLUTE_MODE:
    _drawdifficon(a, FALSE);
    translate(a->w/2.0, 0.0, 0.0);
    _drawabsicon(a, TRUE);
    break;
  case DIFFERENTIAL_MODE:
    _drawdifficon(a, TRUE);
    translate(a->w/2.0, 0.0, 0.0);
    _drawabsicon(a, FALSE);
    break;
  }
  popmatrix();
  popmatrix();

  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
pnl_slideroid(a)
Actuator *a;
{
  Slideroid *ad;

  a->type=PNL_SLIDEROID;

  a->data = (char *)pnl_alloc(sizeof(Slideroid));
  a->datasize = sizeof(Slideroid);
  ad=(Slideroid *)a->data;
  ad->wsave=0.0;
  ad->mode=NORMAL_MODE;

  a->w=PNL_SLIDEROID_WIDTH;

  a->h=PNL_SLIDEROID_HEIGHT;
  a->labeltype=PNL_LABEL_BOTTOM;
  a->newvalfunc=_newvalslideroid;
  a->drawfunc=_drawslideroid;
}

