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

void
_deletemsliderbar(a, sa)
     Actuator *a;
     Actuator *sa;
{
  Multislider *ad=(Multislider *)a->data;
  Actuator *sb;
  
#if 0
  if (a->al==sa) {
    a->al=sa->next;
    /* freeact(sa); */
  } else for (sb=a->al;sb;sb=sb->next)
    if (sb->next==sa) {
      sb->next=sa->next;
      /* freeact(sa); eventually */
      break;
    }
#else
  pnl_delact(sa);
#endif
}

void
setsubactextval(a, sa, x, dist)
     Actuator *a, *sa;
     Coord x;
     float dist;
{
  Actuator *sb;
  Multislider *ad=(Multislider *)a->data;
  float tmp;

  if (pnl_justdown) {
    if (!pnl_controlkey)
      sa->extval=PNL_WORLD_TO_VAL(x, a->minval, a->maxval, dist);
    ad->wsave=x;
  }
  
  sa->extval+=
    (pnl_controlkey?ad->finefactor
      *(PNL_WORLD_TO_VAL(x-ad->wsave, a->minval, a->maxval, dist)-a->minval)
      :(PNL_WORLD_TO_VAL(x-ad->wsave, a->minval, a->maxval, dist)-a->minval));
  sa->extval=RANGE(sa->extval, a->maxval, a->minval);

  if (ad->mode&PNL_MSM_ORDERED) {
    for (sb=a->al;sb!=sa;sb=sb->next)
      sb->extval=MAX(sb->extval, sa->extval);
    for (sb=sa->next;sb;sb=sb->next)
      sb->extval=MIN(sb->extval, sa->extval);
  } else if (ad->mode&PNL_MSM_CONSTRAINED) {
    for (sb=a->al;sb!=sa;sb=sb->next)
      sa->extval=MIN(sb->extval, sa->extval);
    for (sb=sa->next;sb;sb=sb->next)
      sa->extval=MAX(sb->extval, sa->extval);
  }

  ad->wsave=x;
}

void _newvalvmultislider(a,p,x,y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{    
  Multislider *ad=(Multislider *)a->data;
  Actuator *sa;
  float tmp;
  
  if (!a->active) {
    if (a->ca) { 
      a->ca->active=FALSE;
      pnl_newvalact(a->ca,p,x-a->x,y-a->y);
      a->ca=NULL;
      for (sa=a->al;sa;sa=sa->next) pnl_setdirty(sa);
    }
    return;
  }
  
  if (pnl_justdown)
    if (ad->mode&PNL_MSM_ADD) {
      a->ca=pnl_mkact(ad->acttype);
      a->ca->extval=PNL_WORLD_TO_VAL(y-a->y-ad->bh/2.0, a->minval,
				     a->maxval, a->h-ad->bh);
      pnl_addsubact(a->ca, a);
    } else for (sa=a->al;sa;sa=sa->next) 
      if (sa->selectable)
	if (!a->ca) a->ca=sa;
	else
	  if (sa->y+ad->bh/2.0<y-a->y) {
	    if (ABS(sa->y+ad->bh/2.0-(y-a->y))<
		ABS(a->ca->y+ad->bh/2.0-(y-a->y))) a->ca=sa;
	  } else {
	    if (ABS(sa->y+ad->bh/2.0-(y-a->y))<=
		ABS(a->ca->y+ad->bh/2.0-(y-a->y))) a->ca=sa;
	  }

  if (a->ca) {
    if (pnl_justdown&&(ad->mode&PNL_MSM_DELETE)) {
      _deletemsliderbar(a, a->ca);
      a->ca=NULL;
      return;
    }
    setsubactextval(a, a->ca, y-a->y-ad->bh/2.0, a->h-ad->bh);
    for (sa=a->al;sa;sa=sa->next)
      sa->y=PNL_VAL_TO_WORLD(sa->extval,
			     a->minval, a->maxval, a->h-ad->bh);

    a->ca->active=TRUE;
    pnl_newvalact(a->ca,p,x-a->x,y-a->y);
    a->ca->dirtycnt=2;

    ad->sa=a->ca;
    a->val=a->ca->extval;
  }
  pnl_setdirty(a);
}

void _newvalhmultislider(a,p,x,y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{    
  Multislider *ad=(Multislider *)a->data;
  Actuator *sa;
  float tmp;
  
  if (!a->active) {
    if (a->ca) { 
      a->ca->active=FALSE;
      pnl_newvalact(a->ca,p,x-a->x,y-a->y);
      a->ca=NULL;
      for (sa=a;sa;sa=sa->next) pnl_setdirty(sa);
    }
    return;
  }
  
  if (pnl_justdown)
    if (ad->mode&PNL_MSM_ADD) {
      a->ca=pnl_mkact(ad->acttype);
      a->ca->extval=PNL_WORLD_TO_VAL(x-a->x-ad->bh/2.0, a->minval,
				     a->maxval, a->w-ad->bh);
      pnl_addsubact(a->ca, a);
    } else for (sa=a->al;sa;sa=sa->next) 
      if (sa->selectable)
	if (!a->ca) a->ca=sa;
	else
	  if (sa->x+ad->bh/2.0<x-a->x) {
	    if (ABS(sa->x+ad->bh/2.0-(x-a->x))<
		ABS(a->ca->x+ad->bh/2.0-(x-a->x))) a->ca=sa;
	  } else {
	    if (ABS(sa->x+ad->bh/2.0-(x-a->x))<=
		ABS(a->ca->x+ad->bh/2.0-(x-a->x))) a->ca=sa;
	  }

  if (a->ca) {
    if (pnl_justdown&&(ad->mode&PNL_MSM_DELETE)) {
      _deletemsliderbar(a, a->ca);
      a->ca=NULL;
      return;
    }
    setsubactextval(a, a->ca, x-a->x-ad->bh/2.0, a->w-ad->bh);
    for (sa=a->al;sa;sa=sa->next)
      sa->x=PNL_VAL_TO_WORLD(sa->extval,
			     a->minval, a->maxval, a->w-ad->bh);

    a->ca->active=TRUE;
    pnl_newvalact(a->ca,p,x-a->x,y-a->y);
    pnl_setdirty(a->ca);

    ad->sa=a->ca;
    a->val=a->ca->extval;
  }
  pnl_setdirty(a);
}

void
_newvalmsliderbar(a,p,x,y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{    
  (void) printf("_newvalmsliderbar for %s called: x=%f, y=%f\n",
	 (a->label?a->label:"(nil)"), x, y);
  pnl_setdirty(a);
}

void
  setclearregion(a)
Actuator *a;
{
  Actuator *sa;
  Multislider *ad=(Multislider *)a->data;
  
  ad->clrx=ad->clry=1000000.0;
  ad->clrw=ad->clrh= -1000000.0;
  
  for (sa=a->al;sa;sa=sa->next) {
    if (sa->label) {
      ad->clrx=MIN(ad->clrx, sa->lx);
      ad->clrw=MAX(ad->clrw, a->w-sa->w+sa->lw);
      ad->clry=MIN(ad->clry, sa->ly);
      ad->clrh=MAX(ad->clrh, a->h-sa->h+sa->lh);
    }
  }
  if (ad->clrx==1000000.0) ad->clrx=0.0;
  if (ad->clry==1000000.0) ad->clry=0.0;
  if (ad->clrw==-1000000.0) ad->clrw=0.0;
  if (ad->clrh==-1000000.0) ad->clrh=0.0;
}

void
  _fixvmultislider(a)
Actuator *a;
{
  Actuator *sa;
  Multislider *ad=(Multislider *)a->data;
  
  setclearregion(a);
  for (sa=a->al;sa;sa=sa->next) 
    sa->y=PNL_VAL_TO_WORLD(sa->extval, a->minval, a->maxval, a->h-ad->bh);
}

void
  _fixhmultislider(a)
Actuator *a;
{
  Actuator *sa;
  Multislider *ad=(Multislider *)a->data;
  
  setclearregion(a);
  for (sa=a->al;sa;sa=sa->next)
    sa->x=PNL_VAL_TO_WORLD(sa->extval, a->minval, a->maxval, a->w-ad->bh);
}

void
_drawmultislider(a, p)
Actuator *a;
Panel *p;
{
  Multislider *ad=(Multislider *)a->data;
  Actuator *sa;

  for (sa=a->al;sa;sa=sa->next)
    sa->dirtycnt=MAX(sa->dirtycnt, a->dirtycnt);

  pushmatrix();

  translate(a->x,a->y,0.0);

  if (a->dirtycnt) {
    color(pnl_background_color);
    rectf(ad->clrx, ad->clry, ad->clrx+ad->clrw, ad->clry+ad->clrh);
    color(pnl_normal_color);
    rectf(0.0,0.0,a->w,a->h);
    color(pnl_black_color);
    rect(0.0,0.0,a->w,a->h);
  }

  for (sa=a->al;sa;sa=sa->next) {
#ifdef DEBUG
    (void) printf("drawing %s from _drawvmultislider\n", sa->label);
#endif
    pnl_drawact(sa, a->p);
  }

  popmatrix();
  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);

#if 0
/* turn this code on to check location of clearing boxes */
  pushmatrix();
  translate(a->x,a->y,0.0);

  color(pnl_normal_color);
  rect(a->lx, a->ly, a->lx+a->lw, a->ly+a->lh);
  color(pnl_black_color);
  rect(ad->clrx, ad->clry, ad->clrx+ad->clrw, ad->clry+ad->clrh);

  popmatrix();
#endif
}

void
_drawmsliderbar(a, p)
Actuator *a;
Panel *p;
{
  if (!a->dirtycnt) return;

  color(pnl_highlight_color);
  rectf(a->x,a->y,a->x+a->w,a->y+a->h);
  color(pnl_black_color);
  rect(a->x,a->y,a->x+a->w,a->y+a->h);
  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
_drawmslideropenbar(a, p)
Actuator *a;
Panel *p;
{
  if (!a->dirtycnt) return;

  color(pnl_normal_color);
  rectf(a->x,a->y,a->x+a->w,a->y+a->h);
  color(pnl_black_color);
  rect(a->x,a->y,a->x+a->w,a->y+a->h);
  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
_addmultislider(a)
     Actuator *a;
{
  Actuator *sa;
  int i;
  Multislider *ad=(Multislider *)a->data;
  char buf[20];
  float maxx, minx;

  for (i=0;i<ad->n;i++) {
    sa=pnl_mkact(ad->acttype);
    (void) sprintf(buf, "%d", i);
    sa->label=(char *)pnl_alloc(strlen(buf)+1);
    (void) strcpy(sa->label, buf);
    sa->extval=PNL_WORLD_TO_VAL((i+.5), a->minval, a->maxval,(float) ad->n);
    pnl_addsubact(sa, a);
  }
  pnl_fixact(a);
}

void
_addsubvmultislider(sa, a)
     Actuator *sa, *a;
{
  Multislider *ad=(Multislider *)a->data;
  Actuator *sb, *sc;

  a->na++;
  sa->pa=a;

  sa->extval=RANGE(sa->extval, a->minval, a->maxval);
  sa->w=a->w;
  sa->h=ad->bh;
  sa->y=PNL_VAL_TO_WORLD(sa->extval, a->minval, a->maxval, a->h-ad->bh); /* ??? */

  if ((ad->mode&PNL_MSM_ORDERED)||(ad->mode&PNL_MSM_CONSTRAINED)) {
    if (!a->al||(a->al->extval<sa->extval)) {
      sa->next=a->al;
      a->al=sa;
    } else {
      for (sc=a->al,sb=a->al->next;sb;sc=sb,sb=sb->next)
	if (sb->extval<=sa->extval) break;
      sa->next=sc->next;
      sc->next=sa;
    }
  } else {
    sa->next=a->al;
    a->al=sa;
  }
  a->ca=sa;
  pnl_fixact(a);
}

void
_addsubhmultislider(sa, a)
     Actuator *sa, *a;
{
  Multislider *ad=(Multislider *)a->data;
  Actuator *sb, *sc;

  a->na++;
  sa->pa=a;

  sa->extval=RANGE(sa->extval, a->minval, a->maxval);
  sa->h=a->h;
  sa->w=ad->bh;
  sa->x=PNL_VAL_TO_WORLD(sa->extval, a->minval, a->maxval, a->w-ad->bh); /* ??? */

  if ((ad->mode&PNL_MSM_ORDERED)||(ad->mode&PNL_MSM_CONSTRAINED)) {
    if (!a->al||(a->al->extval<sa->extval)) {
      sa->next=a->al;
      a->al=sa;
    } else {
      for (sc=a->al,sb=a->al->next;sb;sc=sb,sb=sb->next)
	if (sb->extval<=sa->extval) break;
      sa->next=sc->next;
      sc->next=sa;
    }
  } else {
    sa->next=a->al;
    a->al=sa;
  }
  a->ca=sa;
  pnl_fixact(a);
}

void
_fixvmsliderbar(a)
     Actuator *a;
{
  Multislider *ad=(Multislider *)a->pa->data;

  a->y=PNL_VAL_TO_WORLD(RANGE(a->extval, a->pa->minval, a->pa->maxval),
			a->pa->minval,
			a->pa->maxval,
			a->pa->h-ad->bh);
}

void
_fixhmsliderbar(a)
     Actuator *a;
{
  Multislider *ad=(Multislider *)a->pa->data;

  a->x=PNL_VAL_TO_WORLD(RANGE(a->extval, a->pa->minval, a->pa->maxval),
			a->pa->minval,
			a->pa->maxval,
			a->pa->w-ad->bh);
}

void
pnl_vmultislider(a)
Actuator *a;
{
  Multislider *ad;

  a->type=PNL_MULTISLIDER;

  a->data = (char *)pnl_alloc(sizeof(Multislider));
  a->datasize = sizeof(Multislider);
  ad=(Multislider *)a->data;
  ad->finefactor=PNL_FINE_CONTROL_FACTOR;
  ad->wsave=0.0;
  ad->n=PNL_MULTISLIDER_DIVISIONS;
  ad->acttype=pnl_vmultislider_bar;
  ad->bh=PNL_SLIDER_BAR_HEIGHT/2.0;

  a->labeltype=PNL_LABEL_BOTTOM;

  a->addfunc=_addmultislider;
  a->fixfunc=_fixvmultislider;
  a->addsubfunc=_addsubvmultislider; /* to add a sub act to this act */

  a->w=PNL_SLIDER_WIDTH;
  a->h=PNL_SLIDER_HEIGHT;
  a->newvalfunc=_newvalvmultislider;
  a->drawfunc=_drawmultislider;
}

void
pnl_vmultislider_bar(a)
Actuator *a;
{
  pnl_button(a);

  a->type=PNL_VMULTISLIDER_BAR;

  a->w=PNL_SLIDER_WIDTH;
  a->h=PNL_SLIDER_BAR_HEIGHT/2.0;
  a->labeltype=PNL_LABEL_RIGHT;
  a->fixfunc=_fixvmsliderbar;

  a->drawfunc=_drawmsliderbar;
}

void
pnl_vmultislider_open_bar(a)
Actuator *a;
{
  pnl_vmultislider_bar(a);
  a->type=PNL_MULTISLIDER_OPEN_BAR;
  a->drawfunc=_drawmslideropenbar;
}

void
pnl_multislider(a)
Actuator *a;
{
  pnl_vmultislider(a);
}

void
pnl_multislider_bar(a)
Actuator *a;
{
  pnl_vmultislider_bar(a);
}

void
pnl_multislider_open_bar(a)
Actuator *a;
{
  pnl_vmultislider_open_bar(a);
}

void
pnl_hmultislider(a)
Actuator *a;
{
  Multislider *ad;

  a->type=PNL_HMULTISLIDER;

  a->data = (char *)pnl_alloc(sizeof(Multislider));
  a->datasize = sizeof(Multislider);
  ad=(Multislider *)a->data;
  ad->finefactor=PNL_FINE_CONTROL_FACTOR;
  ad->wsave=0.0;
  ad->n=PNL_MULTISLIDER_DIVISIONS;
  ad->acttype=pnl_hmultislider_bar;
  ad->bh=PNL_SLIDER_BAR_HEIGHT/2.0;

  a->labeltype=PNL_LABEL_BOTTOM;

  a->addfunc=_addmultislider;
  a->fixfunc=_fixhmultislider;
  a->addsubfunc=_addsubhmultislider; /* to add a sub act to this act */

  a->w=PNL_SLIDER_HEIGHT;
  a->h=PNL_SLIDER_WIDTH;
  a->newvalfunc=_newvalhmultislider;
  a->drawfunc=_drawmultislider;
}

void
pnl_hmultislider_bar(a)
Actuator *a;
{
  pnl_button(a);

  a->type=PNL_HMULTISLIDER_BAR;

  a->w=PNL_SLIDER_BAR_HEIGHT/2.0;
  a->h=PNL_SLIDER_WIDTH;
  a->labeltype=PNL_LABEL_TOP;
  a->fixfunc=_fixhmsliderbar;

  a->drawfunc=_drawmsliderbar;
}

void
pnl_hmultislider_open_bar(a)
Actuator *a;
{
  pnl_hmultislider_bar(a);
  a->type=PNL_HMULTISLIDER_OPEN_BAR;
  a->drawfunc=_drawmslideropenbar;
}


