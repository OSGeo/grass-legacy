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

#define INTERP(x1, x2, a) ((a)*(x2)+(1.0-(a))*(x1))

void
_newvalviewframe(a,p,x,y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{
  Viewframe *ad=(Viewframe *)a->data;

  /* frame coords (relative to rotation origin of this frame */
  Coord fx=x-a->x;
  Coord fy=y-a->y;
  Screencoord sx, sy;
  
  Actuator *sa;
  Coord wx, wy;

  {
    float alpha;
    Coord wx1, wy1, wz1, wx2, wy2, wz2;
    mapwfind(ad->vobj, fx, fy, &wx1, &wy1, &wz1, &wx2, &wy2, &wz2);
    alpha=wz2/(wz2-wz1);	/* intersect at z==0 */
    wx=INTERP(wx2, wx1, alpha);
    wy=INTERP(wy2, wy1, alpha);
  }

  /* apply tweaks in panel coordinate frame */
  wx/=a->scalefactor;
  wy/=a->scalefactor;
  wx+=ad->minx;
  wy+=ad->miny;

  if (!a->active) {
    if (a->ca) { 
      a->ca->active=FALSE;
      pnl_newvalact(a->ca,p,wx,wy);
      a->ca=NULL;
    }
    return;
  }

  if (pnl_justdown)
    for (sa=a->al;sa;sa=sa->next)
      if (sa->selectable&&sa->visible&&(*sa->pickfunc)(sa, p, wx, wy)) {
	sa->active=TRUE;
	a->ca=sa;
	break;
      } 
  
  if (a->ca) {
    pnl_newvalact(a->ca,p,wx,wy);
  }
  
  if (a->ca) a->val=a->ca->val;
}

Boolean
_hitviewframe(a, p, x, y)
Actuator *a;
Panel *p;
Coord x,y;
{
  Actuator *sa;
  Viewframe *ad=(Viewframe *)a->data;
  Coord fx=x-a->x;
  Coord fy=y-a->y;
  Coord wx, wy;

  {
    float alpha;
    Coord wx1, wy1, wz1, wx2, wy2, wz2;

    mapwfind(ad->vobj, fx, fy, &wx1, &wy1, &wz1, &wx2, &wy2, &wz2);
    alpha=wz2/(wz2-wz1);
    wx=INTERP(wx2, wx1, alpha);
    wy=INTERP(wy2, wy1, alpha);
  }
  
  if (wx>0 && wx<a->w && wy>0 && wy<a->h) return TRUE;

  wx/=a->scalefactor;
  wy/=a->scalefactor;
  wx+=ad->minx;
  wy+=ad->miny;

  for (sa=a->al;sa;sa=sa->next)
    if (sa->selectable&&sa->visible&&(*sa->pickfunc)(sa, p, wx, wy)) 
      return TRUE;

  return FALSE;
}


void
viewframelimits(a)
     Actuator *a;
{
  float minx, miny, maxx, maxy;
  Viewframe *ad=(Viewframe *)a->pa->data;
  
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
_addsubviewframe(sa, a)
     Actuator *sa, *a;
{
  Viewframe *ad=(Viewframe *)a->data;

  a->na++;
  sa->pa=a;

  sa->next=a->al;
  a->al=sa;

  pnl_fixact(a);
}

void
_addviewframe(a)
     Actuator *a;
{
  Actuator *sa;
  Viewframe *ad=(Viewframe *)a->data;

  pnl_fixact(a);
}

makeviewobj(a)
Actuator *a;
{
  Viewframe *ad=(Viewframe *)a->data;

  editobj(ad->vobj);
  objdelete(STARTTAG, ENDTAG);
  objinsert(STARTTAG);
  maketag(PNL_VFT_TRANSLATE1);
  translate(ad->tx1, ad->ty1, ad->tz1);
  maketag(PNL_VFT_ROTATE_X1);
  rot(ad->rotx1, 'x');
  maketag(PNL_VFT_ROTATE_Z);
  rot(ad->rotz, 'z');
  maketag(PNL_VFT_ROTATE_X2);
  rot(ad->rotx2, 'x');
  maketag(PNL_VFT_TRANSLATE2);
  translate(ad->tx2, ad->ty2, ad->tz2);
  maketag(PNL_VFT_SCALE);
  scale(ad->sx, ad->sy, ad->sz);
  closeobj();
}

replaceviewobj(a)
Actuator *a;
{
  Viewframe *ad=(Viewframe *)a->data;

  editobj(ad->vobj);
  objreplace(PNL_VFT_TRANSLATE1);
  translate(ad->tx1, ad->ty1, ad->tz1);
  objreplace(PNL_VFT_ROTATE_X1);
  rot(ad->rotx1, 'x');
  objreplace(PNL_VFT_ROTATE_Z);
  rot(ad->rotz, 'z');
  objreplace(PNL_VFT_ROTATE_X2);
  rot(ad->rotx2, 'x');
  objreplace(PNL_VFT_TRANSLATE2);
  translate(ad->tx2, ad->ty2, ad->tz2);
  objreplace(PNL_VFT_SCALE);
  scale(ad->sx, ad->sy, ad->sz);
  closeobj();
}

void
_fixviewframe(a)
     Actuator *a;
{
  Actuator *sa;
  Viewframe *ad=(Viewframe *)a->data;

  if (a->al) {
    ad->minx=100000.0;
    ad->miny=100000.0;
    ad->maxx= -100000.0;
    ad->maxy= -100000.0;
    for (sa=a->al;sa;sa=sa->next) viewframelimits(sa);
  } else {
    ad->minx=ad->miny=ad->maxx=ad->maxy=0;
  }
  ad->minx-=PNL_MARGIN;
  ad->maxx+=PNL_MARGIN;
  ad->miny-=PNL_MARGIN;
  ad->maxy+=PNL_MARGIN;

/* switch on mode here to constrain by ppu or frame dimensions */

  if (ad->mode&PNL_FM_FIXED) {
    /* don't adjust anything */
  } else if (ad->mode&PNL_FM_FIXED_SIZE) {
    a->scalefactor=MIN(a->w/(ad->maxx-ad->minx),
		       a->h/(ad->maxy-ad->miny));
  } else {
    a->w=a->scalefactor*(ad->maxx-ad->minx);
    a->h=a->scalefactor*(ad->maxy-ad->miny);
  }

  replaceviewobj(a);
}

void
_drawviewframe(a, p)
     Actuator *a;
     Panel *p;
{
  Actuator *sa;
  Viewframe *ad=(Viewframe *)a->data;
  
  for (sa=a->al;sa;sa=sa->next)
    sa->dirtycnt=MAX(sa->dirtycnt, a->dirtycnt);

  pushmatrix();
  translate(a->x, a->y, 0.0);

  callobj(ad->vobj);

  scale(a->scalefactor, a->scalefactor, a->scalefactor);
  translate(-ad->minx, -ad->miny, 0.0);

  if (a->dirtycnt) {
    color(pnl_background_color);

    rectf(ad->minx, ad->miny, ad->maxx, ad->maxy);


    translate(0.0, 0.0, -PNL_DIM_3);
    color(pnl_black_color);
    move2(ad->minx, ad->miny);
    draw2(ad->maxx, ad->maxy);
    move2(ad->minx, ad->maxy);
    draw2(ad->maxx, ad->miny);
    translate(0.0, 0.0, PNL_DIM_3);

    if (!a->beveled) {
      color(pnl_black_color);
      rect(ad->minx, ad->miny, ad->maxx, ad->maxy);
    }
  }
  
  translate(-ad->offx, -ad->offy, 0.0);
  for (sa=a->al;sa;sa=sa->next) pnl_drawact(sa, p);
  
  if (a->beveled) {
      translate(ad->minx, ad->miny, 0.0);
      scale(1.0/a->scalefactor, 1.0/a->scalefactor, 1.0/a->scalefactor);
      translate(-a->x, -a->y, 0.0);
      pnl_drawbevel(a, p);
      if (a->label) pnl_drawlabel(a, p);
    }

  popmatrix();
}

void
pnl_viewframe(a)
     Actuator *a;
{
  Viewframe *ad;
  a->type=PNL_FRAME;
  
  a->data = (char *) pnl_alloc(sizeof(Viewframe));
  a->datasize = sizeof(Viewframe);
  ad=(Viewframe *)a->data;
  ad->mode=PNL_FM_FREE;
  ad->offx=0;
  ad->offy=0;
  ad->tx1=ad->ty1=ad->tz1=0;
  ad->rotx1=ad->rotz=ad->rotx2=0;
  ad->tx2=ad->ty2=ad->tz2=0;
  ad->sx=ad->sy=ad->sz=1;

  makeobj(ad->vobj=genobj());
  makeviewobj(a);

  a->w=PNL_DIM_1;
  a->h=PNL_DIM_1;
  a->labeltype=PNL_LABEL_TOP;
  a->newvalfunc=_newvalviewframe;
  a->pickfunc=_hitviewframe;
  a->fixfunc=_fixviewframe;
  a->addfunc=_addviewframe;
  a->addsubfunc=_addsubviewframe;
  a->drawfunc=_drawviewframe;
}

