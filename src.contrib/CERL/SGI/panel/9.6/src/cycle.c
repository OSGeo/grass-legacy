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
_shiftcycleright(a)
     Actuator *a;
{
  Actuator *cycle=a->pa->pa;
  Cycle *ad=(Cycle *)cycle->data;

  if (!ad->memberlist) return;

  ad->currentmember->a->visible=FALSE;

  if (ad->currentmember->next) ad->currentmember=ad->currentmember->next;
  else			       ad->currentmember=ad->memberlist;

  ad->currentmember->a->visible=TRUE;

  pnl_fixact(cycle);
  pnl_fixpanel(cycle->p);
}

void
_shiftcycleleft(a)
     Actuator *a;
{
  Alist *e;
  Actuator *cycle=a->pa->pa;
  Cycle *ad=(Cycle *)cycle->data;

  if (!ad->memberlist) return;

  ad->currentmember->a->visible=FALSE;

  if (ad->currentmember->a==ad->memberlist->a) {
    for (e=ad->memberlist;e->next;e=e->next);
  } else {
    for (e=ad->memberlist;e->next->a!=ad->currentmember->a;e=e->next);
  }
  ad->currentmember=e;

  ad->currentmember->a->visible=TRUE;

  pnl_fixact(cycle);
  pnl_fixpanel(cycle->p);
}

void
_newvalcycle(a,p,x,y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{
  Cycle *ad=(Cycle *)a->data;

  if (!a->active) {
    ad->frame->active=FALSE;
    pnl_newvalact(ad->frame,p,x-a->x,y-a->y);
    return;
  }
  
  if (pnl_justdown)
    ad->frame->active=TRUE;
  
  pnl_newvalact(ad->frame,p,x-a->x,y-a->y);
  
  a->val=ad->frame->val;
}

void
_addcycle(a)
     Actuator *a;
{
}

void
  _addsubcycle(sa, a)
Actuator *sa, *a;
{
  Cycle *ad=(Cycle *)a->data;
  Alist *e, *f, *g;
  
  if (ad->mode==PNL_CM_UNDER_CONSTRUCTION) {
    a->na++;
    sa->pa=a;
  } else {
    e=(Alist *)pnl_alloc(sizeof(Alist));
    e->a=sa;
    e->next=NULL;
    
    /* append to end of list */
    if (ad->memberlist) {
      for (g=ad->memberlist,
	   f=ad->memberlist->next;f;g=f,f=f->next);
      e->a->visible=FALSE;
      g->next=e;
    } else {
      e->a->visible=TRUE;
      ad->memberlist=ad->currentmember=e;
    }
    
    pnl_fixact(sa);
    ad->frame->w=MAX(ad->frame->w, sa->w+2*PNL_DIM_1);
    ad->frame->h=MAX(ad->frame->h,
		     MAX(ad->shiftleftbutton->h, ad->shiftrightbutton->h)+
		     sa->h+
		     3*PNL_DIM_1);
    (ad->frame->addsubfunc)(sa, ad->frame);
    
    pnl_fixact(a);
  }
}

void
_fixcycle(a)
     Actuator *a;
{
  Cycle *ad=(Cycle *)a->data;
  Actuator *cma;

  if (ad->currentmember&&ad->currentmember->a)
    cma=ad->currentmember->a;	/* current member actuator */
  else
    return;

  ad->shiftleftbutton->x=PNL_ACCESS(Frame, ad->frame, maxx)-
    (ad->shiftleftbutton->w+ad->shiftrightbutton->w+PNL_DIM_2+PNL_DIM_1);
  ad->shiftrightbutton->x=PNL_ACCESS(Frame, ad->frame, maxx)-
      (ad->shiftrightbutton->w+PNL_DIM_1);
  ad->shiftleftbutton->y=ad->shiftrightbutton->y=
    cma->y+cma->h+PNL_DIM_1;

  pnl_fixact(ad->frame);

  a->w=ad->frame->w;
  a->h=ad->frame->h;
}

void
_drawcycle(a, p)
     Actuator *a;
     Panel *p;
{
  Cycle *ad=(Cycle *)a->data;
  
  ad->frame->dirtycnt=MAX(a->dirtycnt, ad->frame->dirtycnt);

  pushmatrix();
  translate(a->x,a->y,0.0);
  pnl_drawact(ad->frame, ad->frame->p);
  popmatrix();

  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
pnl_cycle(a)
Actuator *a;
{
  Cycle *ad;
  Actuator *sa;
  Coord x=0;

  a->type=PNL_CYCLE;
  a->p=pnl_cp;

  a->data = (char *) pnl_alloc(sizeof(Cycle));
  a->datasize = sizeof(Cycle);
  ad=(Cycle *)a->data;
  ad->mode=PNL_CM_UNDER_CONSTRUCTION;

  a->labeltype=PNL_LABEL_TOP;

  a->newvalfunc=_newvalcycle;
  a->drawfunc=_drawcycle;
  a->addfunc=_addcycle;
  a->fixfunc=_fixcycle;
  a->addsubfunc=_addsubcycle; /* to add a sub act to this act */

  a->ca=ad->frame=sa=pnl_mkact(pnl_frame);
  PNL_ACCESS(Frame, sa, mode)=PNL_FM_FIXED;
  pnl_addsubact(sa, a);

  ad->shiftleftbutton=sa=pnl_mkact(pnl_left_arrow_button);
  sa->downfunc=_shiftcycleleft;
  pnl_addsubact(sa, ad->frame);

  x+=sa->w+PNL_DIM_3;

  ad->shiftrightbutton=sa=pnl_mkact(pnl_right_arrow_button);
  sa->x=x;
  sa->downfunc=_shiftcycleright;
  pnl_addsubact(sa, ad->frame);

  ad->frame->w=ad->shiftrightbutton->w+
    ad->shiftleftbutton->w+
      3*PNL_DIM_2;
  ad->frame->h=MAX(ad->shiftrightbutton->h,
	   ad->shiftleftbutton->h)+2*PNL_DIM_2;

  ad->mode=PNL_CM_NORMAL;
}

