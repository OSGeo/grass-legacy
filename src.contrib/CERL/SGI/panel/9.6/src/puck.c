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
  
void _newvalpuck(a,p,x,y)
Actuator *a;
Panel *p;
Coord x, y;
{
  static Coord d=PNL_PUCK_SIZE/2.0;
  
  if (!a->active) return;
  
  x=RANGE(x-a->x, d, a->w-d);
  y=RANGE(y-a->y, d, a->h-d);
  PNL_ACCESS(Puck, a, x)=(x-d)/(a->w-2.0*d)*(a->maxval-a->minval)+a->minval;
  PNL_ACCESS(Puck, a, y)=(y-d)/(a->h-2.0*d)*(a->maxval-a->minval)+a->minval;
  pnl_setdirty(a);
}

void
_drawpuckshape(a, p, x, y, style)
Actuator *a;
Panel *p;
Coord x, y;
int style;
{
static Coord d=PNL_PUCK_SIZE/2.0;

  if (style==PNL_FILLED) {
    pmv2(-d,0.0);
    pdr2(-PNL_DIM_2,-PNL_DIM_2);
    pdr2(PNL_DIM_2,-PNL_DIM_2);
    pdr2(d,0.0);
    pdr2(PNL_DIM_2,PNL_DIM_2);
    pdr2(-PNL_DIM_2,PNL_DIM_2);
    pclos();
    pmv2(0.0,-d);
    pdr2(PNL_DIM_2,-PNL_DIM_2);
    pdr2(PNL_DIM_2,PNL_DIM_2);
    pdr2(0.0,d);
    pdr2(-PNL_DIM_2,PNL_DIM_2);
    pdr2(-PNL_DIM_2,-PNL_DIM_2);
    pclos();
  } else {
    move2(-d,0.0);
    draw2(-PNL_DIM_2, -PNL_DIM_2);
    draw2(0.0,-d);
    draw2(PNL_DIM_2,-PNL_DIM_2);
    draw2(d,0.0);
    draw2(PNL_DIM_2,PNL_DIM_2);
    draw2(0.0,d);
    draw2(-PNL_DIM_2,PNL_DIM_2);
    draw2(-d,0.0);
  }
}

void
_drawpuck(a, p)
Actuator *a;
Panel *p;
{
Coord x,y;
static Coord d = PNL_PUCK_SIZE/2.0;

  if (!a->dirtycnt) return;

    PNL_ACCESS(Puck, a, x)=RANGE(PNL_ACCESS(Puck, a, x), a->minval, a->maxval);
    PNL_ACCESS(Puck, a, y)=RANGE(PNL_ACCESS(Puck, a, y), a->minval, a->maxval);

    color(pnl_normal_color);
    rectf(a->x,a->y,a->x+a->w,a->y+a->h);

    pushmatrix();
    x=a->w*(PNL_ACCESS(Puck, a, x)-a->minval)/(a->maxval-a->minval);
    y=a->h*(PNL_ACCESS(Puck, a, y)-a->minval)/(a->maxval-a->minval);

    translate(a->x+d, a->y+d, 0.0);
    translate(x*(a->w-PNL_PUCK_SIZE)/a->w,y*(a->h-PNL_PUCK_SIZE)/a->h,0.0);

    color(pnl_highlight_color);
    _drawpuckshape(a,p,x,y,PNL_FILLED);
    color(pnl_black_color);
    _drawpuckshape(a,p,x,y,PNL_OPEN);
    popmatrix();

    color(pnl_black_color);
    rect(a->x,a->y,a->x+a->w,a->y+a->h);
    if (a->beveled) pnl_drawbevel(a, p);
    if (a->label) pnl_drawlabel(a, p);
}

void
  _newvalfloatingpuck(a,p,x,y)
Actuator *a;
Panel *p;
Coord x, y;
{
  static Coord d=PNL_PUCK_SIZE/2.0;
  
  if (!a->active) {
    pnl_setdirty(a);
    return;
  }
  
  /* could use PNL_WORLD_TO_VAL here */
  PNL_ACCESS(Puck, a, x)=(x-a->x-d)/PNL_FLOATING_PUCK_SENS
    *(a->maxval-a->minval);
  PNL_ACCESS(Puck, a, y)=(y-a->y-d)/PNL_FLOATING_PUCK_SENS
    *(a->maxval-a->minval);

  pnl_setdirty(a);
}

#define PUCK_ERASE  0
#define PUCK_DRAW   1

/*
 *  drawpup	erase and draw in pup planes
 *
 *	caveats:
 *
 *	    for erasing to work correctly, subsequent calls
 *	    must be made with the same drawfunc.
 *
 *	    on the 3030, the cursor should be turned off.
 *
 */

_drawpup(a,p,x,y,mode,drawfunc)
Actuator *a;  /*p*/
Panel *p;
Coord x,y;
int mode;
void (* drawfunc)();
{
long savemode;
static Boolean drawn=FALSE;
static Coord ox=0.0,oy=0.0,ox2=0.0,oy2=0.0;

	if (mode==PUCK_DRAW
	    && ox==x
	    && oy==y
	    && drawn) return;
#ifdef IRIS_4D
	savemode=getdrawmode();
	drawmode(PUPDRAW);
#else  IRIS_4D
	pupmode();
#endif IRIS_4D

	pushmatrix();
	pushviewport();
	fullscrn();
	ortho2(p->minx-(p->x/p->ppu),
	       p->minx+((XMAXSCREEN-p->x)/p->ppu),
	       p->miny-(p->y/p->ppu),
	       p->miny+((YMAXSCREEN-p->y)/p->ppu));


#ifdef IRIS_4D
	color(0);
#else  IRIS_4D
	pupcolor(PUP_CLEAR);
#endif IRIS_4D
/*	pushmatrix();
	translate(ox2,oy2,0.0);
	(* drawfunc)(a,p,PNL_FILLED);
	popmatrix(); */

	pushmatrix();
	translate(ox,oy,0.0);
	(* drawfunc)(a,p,ox,oy,PNL_FILLED);
	popmatrix();
	drawn=FALSE;

	if (mode==PUCK_DRAW) {
#ifdef IRIS_4D
	    color(1);
#else  IRIS_4D
	    pupcolor(PUP_CURSOR);
#endif IRIS_4D
	    pushmatrix();
	    translate(x,y,0.0);
	    (*drawfunc)(a,p, x, y, PNL_FILLED);
	    popmatrix();
	    ox2=ox;oy2=oy;
	    ox=x;oy=y;
	    drawn=TRUE;
	}

#ifdef IRIS_4D
	drawmode((Boolean)savemode);
#else  IRIS_4D
	endpupmode();
#endif IRIS_4D

	endfullscrn();
	popviewport();
	popmatrix();
}

void
_drawfloatingpuck(a,p)
Actuator *a;
Panel *p;
{
static Coord d=PNL_PUCK_SIZE/2.0;
Coord x=0.0, y=0.0;

  if (!a->dirtycnt) return;

    pushmatrix();
    if (!a->active) {
	_drawpup(a,p,x,y,PUCK_ERASE,_drawpuckshape);

	translate(a->x,a->y,0.0);
	color(pnl_normal_color);
        rectf(0.0,0.0,a->w,a->h);
	color(pnl_black_color);
        rect(0.0,0.0,a->w,a->h);
	color(pnl_highlight_color);
	translate(a->w/2.0,a->h/2.0,0.0);
	_drawpuckshape(a,p,x,y,PNL_FILLED);
	color(pnl_black_color);
	_drawpuckshape(a,p,x,y,PNL_OPEN);
	curson();
    } else {
	cursoff();
	x=PNL_FLOATING_PUCK_SENS
	    *PNL_ACCESS(Puck, a, x)/(a->maxval-a->minval)
	    +a->x+a->w/2.0;
	y=PNL_FLOATING_PUCK_SENS
	    *PNL_ACCESS(Puck, a, y)/(a->maxval-a->minval)
	    +a->y+a->h/2.0;

	_drawpup(a,p,x,y,PUCK_DRAW,_drawpuckshape);

	translate(a->x,a->y,0.0);
	color(pnl_highlight_color);
        rectf(0.0,0.0,a->w,a->h);
	color(pnl_black_color);
        rect(0.0,0.0,a->w,a->h);

	color(pnl_normal_color);
	translate(a->w/2.0,a->h/2.0,0.0);
	_drawpuckshape(a,p,x,y,PNL_FILLED);
	color(pnl_black_color);
	_drawpuckshape(a,p,x,y,PNL_OPEN);
    }
    popmatrix();

    color(pnl_black_color);
    if (a->beveled) pnl_drawbevel(a, p);
    if (a->label) pnl_drawlabel(a, p);
}

void
_drawrubberband(a, p, x, y, style)
Actuator *a;
Panel *p;
Coord x, y;
int style;
{
float r, alpha;
Coord dx, dy;

    dx=a->x+a->w/2.0-x;
    dy=a->y+a->h/2.0-y;

    rectf(dx-PNL_DIM_3, 
	  dy-PNL_DIM_3, 
	  dx+PNL_DIM_3, 
	  dy+PNL_DIM_3);

    pushmatrix();
    r=hypot((long float)dx,(long float)dy);
    if (dx==0.0&&dy==0.0) alpha=0.0;
    else		  alpha=atan2(dy,dx);
    alpha=fmod(alpha, 360.0);
    rot(alpha*PNL_RAD_TO_DEG,'z');
    rectf(0.0,-PNL_DIM_3/2.0,r,PNL_DIM_3/2.0);
    popmatrix();
}

void
_drawrubberpuckshape(a, p, x, y, style)
Actuator *a;
Panel *p;
Coord x, y;
int style;
{
static Coord d=PNL_PUCK_SIZE/2.0;

  if (style==PNL_FILLED) {
    rectf(-d,-PNL_DIM_3,d,PNL_DIM_3);
    rectf(-PNL_DIM_3,-d,PNL_DIM_3,d);
    rectf(-PNL_DIM_2,-PNL_DIM_2,PNL_DIM_2,PNL_DIM_2);
  } else {
    move2(-PNL_DIM_3,-d);
    draw2(PNL_DIM_3,-d);
    draw2(PNL_DIM_3,-PNL_DIM_2);
    draw2(PNL_DIM_2,-PNL_DIM_2);
    draw2(PNL_DIM_2,-PNL_DIM_3);
    draw2(d,-PNL_DIM_3);
    draw2(d,PNL_DIM_3);
    draw2(PNL_DIM_2,PNL_DIM_3);
    draw2(PNL_DIM_2,PNL_DIM_2);
    draw2(PNL_DIM_3,PNL_DIM_2);
    draw2(PNL_DIM_3,d);
    draw2(-PNL_DIM_3,d);
    draw2(-PNL_DIM_3,PNL_DIM_2);
    draw2(-PNL_DIM_2,PNL_DIM_2);
    draw2(-PNL_DIM_2,PNL_DIM_3);
    draw2(-d,PNL_DIM_3);
    draw2(-d,-PNL_DIM_3);
    draw2(-PNL_DIM_2,-PNL_DIM_3);
    draw2(-PNL_DIM_2,-PNL_DIM_2);
    draw2(-PNL_DIM_3,-PNL_DIM_2);
    draw2(-PNL_DIM_3,-d);
  }
}

void
_drawrubberpuckshapeandband(a, p, x, y, style)
Actuator *a;
Panel *p;
Coord x, y;
int style;
{
    _drawrubberpuckshape(a, p, x, y, style);
    _drawrubberband(a, p, x, y, style);
}

void
_drawrubberpuck(a,p)
Actuator *a;
Panel *p;
{
static Coord d=PNL_PUCK_SIZE/2.0;
static Coord x, y;

  if (!a->dirtycnt) return;

    pushmatrix();
    if (!a->active) {
	_drawpup(a,p,x,y,PUCK_ERASE,_drawrubberpuckshapeandband);
	translate(a->x,a->y,0.0);
	color(pnl_normal_color);
        rectf(0.0,0.0,a->w,a->h);
	color(pnl_black_color);
        rect(0.0,0.0,a->w,a->h);
	color(pnl_highlight_color);
	translate(a->w/2.0,a->h/2.0,0.0);
	_drawrubberpuckshape(a,p,x,y,PNL_FILLED);
	color(pnl_black_color);
	_drawrubberpuckshape(a,p,x,y,PNL_OPEN);
	curson();
    } else {
	cursoff();
	x=PNL_FLOATING_PUCK_SENS
	    *PNL_ACCESS(Puck, a, x)/(a->maxval-a->minval)
	    +a->x+a->w/2.0;
	y=PNL_FLOATING_PUCK_SENS
	    *PNL_ACCESS(Puck, a, y)/(a->maxval-a->minval)
	    +a->y+a->h/2.0;

	_drawpup(a,p,x,y,PUCK_DRAW,_drawrubberpuckshapeandband);

	translate(a->x,a->y,0.0);
	color(pnl_highlight_color);
        rectf(0.0,0.0,a->w,a->h);
	color(pnl_black_color);
        rect(0.0,0.0,a->w,a->h);

	color(pnl_normal_color);
	translate(a->w/2.0,a->h/2.0,0.0);
	_drawrubberpuckshape(a,p,x,y,PNL_FILLED);
	color(pnl_black_color);
	_drawrubberpuckshape(a,p,x,y,PNL_OPEN);
    }
    popmatrix();
    if (pnl_justup) curson();

    color(pnl_black_color);
    if (a->beveled) pnl_drawbevel(a, p);
    if (a->label) pnl_drawlabel(a, p);
}

void
pnl_puck(a)
Actuator *a;
{
    a->type=PNL_PUCK;

    a->w=a->h=PNL_PUCK_EDGE;
    a->data=(char *)pnl_alloc(sizeof(Puck)); /* this goes in an addfunc */
    a->datasize=sizeof(Puck);
    PNL_ACCESS(Puck, a, x)=0.0;
    PNL_ACCESS(Puck, a, y)=0.0;
    a->labeltype=PNL_LABEL_BOTTOM;
    a->newvalfunc=_newvalpuck;
    a->drawfunc=_drawpuck;
}

void
pnl_floating_puck(a)
Actuator *a;
{
    a->type=PNL_FLOATING_PUCK;

    a->w=PNL_FLOATING_PUCK_EDGE;
    a->h=PNL_FLOATING_PUCK_EDGE;
    a->data=(char *)pnl_alloc(sizeof(Puck));
    a->datasize=sizeof(Puck);
    PNL_ACCESS(Puck, a, x)=0.0;
    PNL_ACCESS(Puck, a, y)=0.0;
    a->newvalfunc=_newvalfloatingpuck;
    a->drawfunc=_drawfloatingpuck;
}

void
pnl_rubber_puck(a)
Actuator *a;
{
    pnl_floating_puck(a);

    a->type=PNL_RUBBER_PUCK;
    a->drawfunc=_drawrubberpuck;
}

