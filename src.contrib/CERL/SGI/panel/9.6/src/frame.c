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
_newvalframe(a,p,x,y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{
  Frame *ad=(Frame *)a->data;
  /* frame coords (relative to origin of this frame */
  Coord fx=(x-a->x+ad->offx)/a->scalefactor+ad->minx;
  Coord fy=(y-a->y+ad->offy)/a->scalefactor+ad->miny;

  Actuator *sa;
  
  if (!a->active) {
    if (a->ca) { 
      a->ca->active=FALSE;
      pnl_newvalact(a->ca,p,fx,fy);
      a->ca=NULL;
    }
    return;
  }

  if (pnl_justdown)
    for (sa=a->al;sa;sa=sa->next)
      if (sa->selectable&&sa->visible&&(*sa->pickfunc)(sa, p, fx, fy)) {
	sa->active=TRUE;
	a->ca=sa;
	break;
      } 
  
  if (a->ca) {
    pnl_newvalact(a->ca,p,fx,fy);
  }
  
  if (a->ca) a->val=a->ca->val;
}

void
framelimits(a)
     Actuator *a;
{
  float minx, miny, maxx, maxy;
  Frame *ad=(Frame *)a->pa->data;
  
  if (!a->visible) return;
    
  minx=a->x;
  miny=a->y;
  maxx=a->x+a->w;
  maxy=a->y+a->h;	/* bounding box */
  
  if (a->label) {
    minx=MIN(minx, a->x+a->lx);
    miny=MIN(miny, a->y+a->ly);
    maxx=a->x+MAX(a->w, a->lx+a->lw);
    maxy=a->y+MAX(a->h, a->ly+a->lh);
  }

  ad->minx=MIN(ad->minx, minx);
  ad->maxx=MAX(ad->maxx, maxx);
  ad->miny=MIN(ad->miny, miny);
  ad->maxy=MAX(ad->maxy, maxy);
}

void
_addsubframe(sa, a)
     Actuator *sa, *a;
{
  Frame *ad=(Frame *)a->data;

  a->na++;
  sa->pa=a;

  sa->next=a->al;
  a->al=sa;

  pnl_fixact(a);
}

void
_addframe(a)
     Actuator *a;
{
  Actuator *sa;
  Frame *ad=(Frame *)a->data;

  pnl_fixact(a);
}

void
_fixframe(a)
     Actuator *a;
{
  Actuator *sa;
  Frame *ad=(Frame *)a->data;

  ad->minx=100000.0;
  ad->miny=100000.0;
  ad->maxx= -100000.0;
  ad->maxy= -100000.0;
  
  for (sa=a->al;sa;sa=sa->next) framelimits(sa);

  ad->minx-=PNL_MARGIN;
  ad->maxx+=PNL_MARGIN;
  ad->miny-=PNL_MARGIN;
  ad->maxy+=PNL_MARGIN;

  if (ad->mode&PNL_FM_FIXED) return;

/* switch on mode here to constrain by ppu or frame dimensions */

  if (ad->mode&PNL_FM_FIXED_SIZE) {
    a->scalefactor=MIN(a->w/(ad->maxx-ad->minx),
		       a->h/(ad->maxy-ad->miny));
  } else {
    a->w=a->scalefactor*(ad->maxx-ad->minx);
    a->h=a->scalefactor*(ad->maxy-ad->miny);
  }
}

#if 0
void
_drawframe(a, p)
     Actuator *a;
     Panel *p;
{
  Actuator *sa;
  Frame *ad=(Frame *)a->data;
  Boolean needtodrawsomething=FALSE;
  Screencoord left=(a->x-p->minx)*p->ppu,
	      right=(a->x+a->w-p->minx)*p->ppu,
	      bottom=(a->y-p->miny)*p->ppu,
	      top=(a->y+a->h-p->miny)*p->ppu;
  
  for (sa=a->al;sa;sa=sa->next)
    sa->dirtycnt=MAX(sa->dirtycnt, a->dirtycnt);

  pushmatrix();
  pushviewport();
  viewport(left, right, bottom, top);

  ortho(ad->minx,ad->maxx,ad->miny,ad->maxy, -100.0, 100.0);

  if (a->dirtycnt) {
    color(pnl_background_color);
    rectf(ad->minx, ad->miny, ad->maxx, ad->maxy);
    if (!a->beveled) {
      color(pnl_black_color);
      rect(ad->minx, ad->miny, ad->maxx, ad->maxy);
    }
  }
  
/*  scale(a->scalefactor, a->scalefactor, 1.0); */

  translate(-ad->offx, -ad->offy, 0.0);

  for (sa=a->al;sa;sa=sa->next) pnl_drawact(sa, p);
  
/*  viewport(p->x, p->x+p->w, p->y, p->y+p->h); */
  popviewport();
  popmatrix();
  
  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}
#endif 0

void
_drawframe(a, p)
     Actuator *a;
     Panel *p;
{
  Actuator *sa;
  Frame *ad=(Frame *)a->data;
  Screencoord left, right, bottom, top;
  
  if (!a->pa) {		/* top level actuator */
    left=(a->x-p->minx)*p->ppu;
    right=(a->x+a->w-p->minx)*p->ppu;
    bottom=(a->y-p->miny)*p->ppu;
    top=(a->y+a->h-p->miny)*p->ppu;
  } else {
    left=(pnl_aox-p->minx)*p->ppu;
    right=(pnl_aox-p->minx+pnl_sf*a->w)*p->ppu;
    bottom=(pnl_aoy-p->miny)*p->ppu;
    top=(pnl_aoy-p->miny+pnl_sf*a->h)*p->ppu;
  }
  
  for (sa=a->al;sa;sa=sa->next)
    sa->dirtycnt=MAX(sa->dirtycnt, a->dirtycnt);

  pushmatrix();
  pushviewport();
  viewport(left, right, bottom, top);

  ortho2(ad->minx,ad->maxx,ad->miny,ad->maxy);

  if (a->dirtycnt) {
    color(pnl_background_color);
    rectf(ad->minx, ad->miny, ad->maxx, ad->maxy);
    if (!a->beveled) {
      color(pnl_black_color);
      rect(ad->minx, ad->miny, ad->maxx, ad->maxy);
    }
  }
  
  translate(-ad->offx, -ad->offy, 0.0);

  for (sa=a->al;sa;sa=sa->next) pnl_drawact(sa, p);

  popviewport();
  popmatrix();
  
  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
pnl_frame(a)
     Actuator *a;
{
  Frame *ad;
  a->type=PNL_FRAME;
  
  a->data = (char *) pnl_alloc(sizeof(Frame));
  a->datasize = sizeof(Frame);
  ad=(Frame *)a->data;
  ad->mode=PNL_FM_FREE;

  a->w=PNL_DIM_1;
  a->h=PNL_DIM_1;
  a->labeltype=PNL_LABEL_TOP;
  a->newvalfunc=_newvalframe;
  a->fixfunc=_fixframe;
  a->addfunc=_addframe;
  a->addsubfunc=_addsubframe;
  a->drawfunc=_drawframe;
}

