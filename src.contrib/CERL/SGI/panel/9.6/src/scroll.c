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

void
_scrollframe(a)
Actuator *a;
{
  Scroll *ad=(Scroll *)a->pa->pa->data;	/* ugh, requires intimate knowlege */
  Frame  *sfd=(Frame  *)ad->subframe->data;

  sfd->offx=ad->hslider->val;
  sfd->offy=ad->vslider->val;
}

void
_newvalscroll(a,p,x,y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{
  Scroll *ad=(Scroll *)a->data;

  if (!a->active) {
    ad->frame->active=FALSE;
    pnl_newvalact(ad->frame,p,x-a->x,y-a->y);
    return;
  }
  
  if (pnl_justdown)
    ad->frame->active=TRUE;
  
  pnl_newvalact(ad->frame,p,x-a->x,y-a->y);
  
  a->ca=ad->subframe->ca;
  a->val=ad->frame->val;

  if (  ad->frame->ca==ad->hslider
      ||ad->frame->ca==ad->vslider) pnl_setdirty(ad->subframe);
}

void
_addsubscroll(sa, a)
     Actuator *sa, *a;
{
  Scroll *ad=(Scroll *)a->data;
  Frame *sfd;
  
  if (!ad->subframe) {
    a->na++;
    sa->pa=a;
  } else {
    sfd=(Frame *)ad->subframe->data;

    (ad->subframe->addsubfunc)(sa, ad->subframe);
    sfd->offx=ad->hslider->val=sfd->minx;
    sfd->offy=ad->vslider->val=sfd->miny;
  }
}

void
_addscroll(a)
     Actuator *a;
{
  Actuator *sa;
  Scroll *ad=(Scroll *)a->data;
  
  pnl_fixact(a);
}

void
_fixscroll(a)
     Actuator *a;
{
  Scroll *ad=(Scroll *)a->data;
  Frame  *fd, *sfd;

  if (!ad->subframe) return;	/* not built yet */

  fd=(Frame  *)ad->frame->data;
  sfd=(Frame  *)ad->subframe->data;

  pnl_fixact(ad->subframe);		/* get mins and maxs */

  ad->frame->x=0;
  ad->frame->y=0;
  ad->frame->w=a->w;
  ad->frame->h=a->h;

  ad->hslider->x=PNL_DIM_2;
  ad->hslider->y=PNL_DIM_2;
  ad->hslider->h=PNL_SLIDER_WIDTH/2.0;
  ad->hslider->w=a->w-ad->hslider->h-3*PNL_DIM_2;

  ad->vslider->w=PNL_SLIDER_WIDTH/2.0;
  ad->vslider->h=a->h-ad->vslider->w-3*PNL_DIM_2;
  ad->vslider->x=a->w-ad->vslider->w-PNL_DIM_2;
  ad->vslider->y=ad->vslider->w+2*PNL_DIM_2;

  ad->subframe->x=PNL_DIM_2;
  ad->subframe->y=ad->hslider->h+2*PNL_DIM_2;
  ad->subframe->w=ad->hslider->w;
  ad->subframe->h=ad->vslider->h;

  pnl_fixact(ad->frame);

  fd->minx=0;
  fd->miny=0;
  fd->maxx=a->w;
  fd->maxy=a->h;	/* no margins */

  ad->hslider->minval=sfd->minx;
  ad->hslider->maxval=sfd->maxx-ad->subframe->w;
  ad->vslider->minval=sfd->miny;
  ad->vslider->maxval=sfd->maxy-ad->subframe->h;

  sfd->minx=0.0;		/* restore limited view */
  sfd->miny=0.0;
  sfd->maxx=ad->subframe->w;
  sfd->maxy=ad->subframe->h;
}

void
_drawscroll(a, p)
     Actuator *a;
     Panel *p;
{
  Actuator *sa;
  Scroll *ad=(Scroll *)a->data;
  
  ad->frame->dirtycnt=MAX(ad->frame->dirtycnt, a->dirtycnt);

  pushmatrix();
  translate(a->x, a->y, 0.0);
  pnl_drawact(ad->frame, p);
  popmatrix();
}

void
pnl_scroll(a)
     Actuator *a;
{
  Scroll *ad;
  Actuator *sa;

  a->type=PNL_SCROLL;
  a->p=pnl_cp;
  
  a->data = (char *) pnl_alloc(sizeof(Scroll));
  a->datasize = sizeof(Scroll);
  ad=(Scroll *)a->data;

  a->w=PNL_SCROLL_WIDTH;
  a->h=PNL_SCROLL_HEIGHT;

  a->labeltype=PNL_LABEL_BOTTOM;
  a->newvalfunc=_newvalscroll;
  a->fixfunc=_fixscroll;
  a->addfunc=_addscroll;
  a->addsubfunc=_addsubscroll;
  a->drawfunc=_drawscroll;

  /* a->ca provides link to active sa */
  /* a->al provides link for drawing */

  a->ca=sa=ad->frame=pnl_mkact(pnl_frame);
  PNL_ACCESS(Frame, sa, mode)=PNL_FM_FIXED;
  sa->x=a->x;
  sa->y=a->y;
  sa->w=a->w;
  sa->h=a->h;
  pnl_addsubact(sa, a);

  sa=ad->hslider=pnl_mkact(pnl_hslider);
  sa->x=PNL_DIM_2;
  sa->y=PNL_DIM_2;
  sa->h/=2.0;	/* half normal slider */
  sa->w=a->w-sa->h-3*PNL_DIM_2;
  sa->activefunc=_scrollframe;
  pnl_addsubact(sa, ad->frame);

  sa=ad->vslider=pnl_mkact(pnl_vslider);
  sa->w/=2.0;
  sa->h=a->h-sa->w-3*PNL_DIM_2;
  sa->x=a->w-sa->w-PNL_DIM_2;
  sa->y=sa->w+2*PNL_DIM_2;
  sa->activefunc=_scrollframe;
  pnl_addsubact(sa, ad->frame);

  sa=ad->subframe=pnl_mkact(pnl_frame);
  PNL_ACCESS(Frame, sa, mode)=PNL_FM_FIXED;
  sa->x=PNL_DIM_2;
  sa->y=ad->hslider->h+2*PNL_DIM_2;
  sa->w=ad->hslider->w;
  sa->h=ad->vslider->h;
  pnl_addsubact(sa, ad->frame);

  PNL_ACCESS(Frame, ad->frame, minx)=0;
  PNL_ACCESS(Frame, ad->frame, miny)=0;
  PNL_ACCESS(Frame, ad->frame, maxx)=a->w;
  PNL_ACCESS(Frame, ad->frame, maxy)=a->h;	/* no margins */
}

