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
#include <stdio.h>
#include <gl.h>
#include <panel.h>

void
_newvalgraphframe(a,p,x,y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{
  Graphframe *ad=(Graphframe *)a->data;
  /* frame coords (relative to origin of this frame */
  Coord fx=(x-a->x)/a->scalefactor;
  Coord fy=(y-a->y)/a->scalefactor;

  ad->x=fx;
  ad->y=fy;
}

void
_addsubgraphframe(sa, a)
     Actuator *sa, *a;
{
  Graphframe *ad=(Graphframe *)a->data;

  fprintf(stderr, "panellib:  warning, can't add subacts to Graphframe\n");
}

void
_drawgraphframe(a, p)
     Actuator *a;
     Panel *p;
{
  Actuator *sa;
  Graphframe *ad=(Graphframe *)a->data;
  Screencoord left, right, bottom, top;
  
  if (!a->dirtycnt) return;

  left=(a->x-p->minx)*p->ppu;
  right=(a->x+a->w-p->minx)*p->ppu;
  bottom=(a->y-p->miny)*p->ppu;
  top=(a->y+a->h-p->miny)*p->ppu;
  
  pushmatrix();
  pushviewport();
  viewport(left, right, bottom, top);

  color(pnl_background_color);
  clear();
  
  if (ad->userdrawfunc) (*ad->userdrawfunc)(a, p);

  popviewport();
  popmatrix();
  
  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
pnl_graphframe(a)
     Actuator *a;
{
  Graphframe *ad;
  a->type=PNL_FRAME;
  
  a->data = (char *) pnl_alloc(sizeof(Graphframe));
  a->datasize = sizeof(Graphframe);
  ad=(Graphframe *)a->data;
  ad->mode=PNL_FM_FREE;

  a->w=PNL_DIM_1;
  a->h=PNL_DIM_1;

  a->labeltype=PNL_LABEL_TOP;
  a->newvalfunc=_newvalgraphframe;
  a->addsubfunc=_addsubgraphframe;
  a->drawfunc=_drawgraphframe;
}

