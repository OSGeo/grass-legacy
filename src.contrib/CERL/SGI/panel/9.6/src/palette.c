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

void setpaletteval(a, x, dist)
     Actuator *a;
     Coord x;
     float dist;
{
  Palette *ad=(Palette *)a->data;

  if (pnl_justdown) {
    a->val=PNL_WORLD_TO_VAL(x, a->minval, a->maxval, dist);
    ad->wsave=x;
  }
  
  if (pnl_controlkey)  a->val+= ad->finefactor
 			 *(x-ad->wsave)/dist*(a->maxval-a->minval);
  else {
    a->val=PNL_WORLD_TO_VAL(x, a->minval, a->maxval, dist);
  }
  a->val=RANGE(a->val, a->maxval, a->minval);
  
  ad->wsave=x;
}

void _newvalvpalette(a,p,x,y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{    
  Palette *ad=(Palette *)a->data;
  float tmp;
  
  if (!a->active) return;
  
  setpaletteval(a, y-a->y, a->h);
  pnl_setdirty(a);
}

void _newvalhpalette(a,p,x,y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{    
  Palette *ad=(Palette *)a->data;
  
  if (!a->active) return;
  
  setpaletteval(a, x-a->x, a->w);
  pnl_setdirty(a);
}

void
_drawvpalette(a, p)
Actuator *a;
Panel *p;
{
Coord y;

  if (!a->dirtycnt) return;

    pushmatrix();

    translate(a->x,a->y,0.0);

    setshade((Colorindex)a->minval);
    pmv2(0.0, 0.0);
    pdr2(a->w, 0.0);
    setshade((Colorindex)a->maxval);
    pdr2(a->w, a->h);
    pdr2(0.0,  a->h);
    spclos();

    color(pnl_black_color);
    rect(0.0,0.0,a->w,a->h);

    popmatrix();
    if (a->beveled) pnl_drawbevel(a, p);
    if (a->label) pnl_drawlabel(a, p);
}

void
_drawhpalette(a, p)
Actuator *a;
Panel *p;
{
Coord y;

  if (!a->dirtycnt) return;

    pushmatrix();

    translate(a->x,a->y,0.0);

    setshade((Colorindex)a->minval);
    pmv2(0.0, 0.0);
    pdr2(0.0, a->h);
    setshade((Colorindex)a->maxval);
    pdr2(a->w, a->h);
    pdr2(a->w, 0.0);
    spclos();
    color(pnl_black_color);
    rect(0.0,0.0,a->w,a->h);

    popmatrix();
    if (a->beveled) pnl_drawbevel(a, p);
    if (a->label) pnl_drawlabel(a, p);
}

void
pnl_palette(a)
Actuator *a;
{
  Palette *ad;

  a->type=PNL_PALETTE;

  a->data = (char *) pnl_alloc(sizeof(Palette));
  a->datasize = sizeof(Palette);
  ad=(Palette *)a->data;
  ad->wsave=0.0;
  ad->finefactor=PNL_FINE_CONTROL_FACTOR;

  a->labeltype=PNL_LABEL_BOTTOM;
  a->w=PNL_SLIDER_WIDTH;
  a->h=PNL_SLIDER_HEIGHT;
  a->newvalfunc=_newvalvpalette;
  a->drawfunc=_drawvpalette;
}

void
pnl_vpalette(a)
Actuator *a;
{
  pnl_palette(a);
  a->type=PNL_VSLIDER;

  a->w=PNL_SLIDER_WIDTH;
  a->h=PNL_SLIDER_HEIGHT;
  a->newvalfunc=_newvalvpalette;
  a->drawfunc=_drawvpalette;
}

void
pnl_hpalette(a)
Actuator *a;
{
  pnl_palette(a);
  a->type=PNL_HSLIDER;

  a->w=PNL_SLIDER_HEIGHT;
  a->h=PNL_SLIDER_WIDTH;
  a->newvalfunc=_newvalhpalette;
  a->drawfunc=_drawhpalette;
}

