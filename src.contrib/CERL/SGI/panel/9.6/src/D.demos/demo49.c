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
#include <device.h>
#include <panel.h>
  
#define PARTICLES	400
#define PARTICLE_SIZE	0.1	/* half-edge */
#define FRAMEMARGIN	2*PARTICLE_SIZE

#define SUN_G		0.05
#define ENV_G		0.05

typedef struct particle {
  float dx, dy;
  Coord x, y;
  float pe;			/* potential energy relative to suns */
  struct particle *next;
} Particle;

typedef struct {
  Actuator *a;
  float attraction;
  struct particle *pl;		/* list of particles */
  Coord wx, wy;			/* a location selected with the mouse */
} Graphworld;

Graphworld graphworld;

Actuator *graphframe, *xgravityslider, *ygravityslider,
  *dragslider, *overallattractionslider,
  *nslider, *repelbutton, 
  *wrapbutton, *smearbutton, *kestrip, *pestrip;

main() 
{
  foreground();
  noport();
  winopen("demo");
  
  doublebuffer();
  gconfig();
  
  defpanel();
  makeworld(&graphworld);
  resetworld(&graphworld);
  
  for (;;) {
    pnl_dopanel();
    animateworld(&graphworld);
    meter(&graphworld);
    swapbuffers();
  }
}

void
  resetworldbyactuator(a)
Actuator *a;
{
  Graphworld *gw=(Graphworld *)a->u;

  resetworld(gw);
}

void
  resetgravity(a)
Actuator *a;
{
  ((Actuator *)a->u)->val=0;
  pnl_fixact((Actuator *)a->u);
}

makeworld(gw)
Graphworld *gw;
{
  Particle *p;
  int i;
  double drand48();

  for (i=0;i<PARTICLES;i++) {
    p=(Particle *)pnl_alloc(sizeof(Particle));
    p->next=gw->pl;
    gw->pl=p;
  }
}

resetworld(gw)
Graphworld *gw;
{
  Particle *p;
  int i;

  for (i=0,p=gw->pl;p&&i<nslider->val;i++,p=p->next) {
    p->x=gw->a->w/2;
    p->y=gw->a->h/2;
    p->dx=drand48()*0.2-0.1;
    p->dy=drand48()*0.2-0.1;
  }
}

void
  reverse(a)
Actuator *a;
{
  Graphworld *gw=(Graphworld *)a->u;
  Particle *p;
  int i;
  
  for (i=0,p=gw->pl;p&&i<nslider->val;i++,p=p->next) {
    p->dx= -1*p->dx;
    p->dy= -1*p->dy;
  }
}
    
animateworld(gw)
     Graphworld *gw;
{
  Particle *p;
  int i;
  float ax, ay, sx, sy, sdx, sdy, t0, dy0;
  
  for (i=0,p=gw->pl;p&&i<nslider->val;i++,p=p->next) {
    
    p->dx+=ax=ENV_G*xgravityslider->val;
    p->dy+=ay=ENV_G*ygravityslider->val;

    p->dx*=1-dragslider->val;
    p->dy*=1-dragslider->val;

    p->x+=p->dx;
    p->y+=p->dy;

    if (wrapbutton->val==1.0) {
      while (p->x<0.0) p->x+=gw->a->w;
      while (p->y<0.0) p->y+=gw->a->h;
      while (p->x>gw->a->w) p->x-=gw->a->w;
      while (p->y>gw->a->h) p->y-=gw->a->h;
    } else {			/* bounce */
      if (p->x<0) {
	p->dx*= -1;
	p->x-= 2*p->x;
      }
      if (p->x>gw->a->w) {
	p->dx*= -1;
	p->x-= 2*(p->x-gw->a->w);
      }
      if (p->y<0.0) {
	p->dy*= -1;
	p->y-= 2*p->y;
      }
      if (p->y>gw->a->h) {
	p->dy*= -1;
	p->y-= 2*(p->y-gw->a->h);
      }
    }
  }
  pnl_fixact(gw->a);
}

void
attraction(a)
     Actuator *a;
{
  Graphworld *gw=(Graphworld *)a->u;
  int i;
  Particle *p;
  float r, r2;
  
  for (i=0,p=gw->pl;p&&i<nslider->val;i++,p=p->next) {
    p->pe=0;
    r2=((gw->wx-p->x)*(gw->wx-p->x))+((gw->wy-p->y)*(gw->wy-p->y));
    r2=MAX(r2, 0.0001);
    p->dx+=repelbutton->val
      *SUN_G*(p->x-gw->wx)*overallattractionslider->val/r2;
    p->dy+=repelbutton->val
      *SUN_G*(p->y-gw->wy)*overallattractionslider->val/r2;
    r=sqrt(r2);
    p->pe+=SUN_G*(-repelbutton->val)*overallattractionslider->val/r;
  }
}

meter(gw)
Graphworld *gw;
{
  int i;
  Particle *p;

  float ke=0, pe=0;
  for (i=0,p=gw->pl;p&&i<nslider->val;i++,p=p->next) {
    ke+=p->dx*p->dx+p->dy*p->dy;
    pe+=p->pe+ENV_G*(p->x*xgravityslider->val+p->y*ygravityslider->val);
  }

  kestrip->val=ke;
  pestrip->val= -pe;

  pnl_fixact(kestrip);
  pnl_fixact(pestrip);
}
  
#define INTERP(x1, x2, a) ((a)*(x2)+(1.0-(a))*(x1))

float
  dist2(x, y, p)
float x, y;
Particle *p;
{
  return (x-p->x)*(x-p->x)+(y-p->y)*(y-p->y);
}

void
newvalgraphframe(a, p, x, y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{
  Viewframe *ad=(Viewframe *)a->data;
  Graphworld *gw=(Graphworld *)a->u;

  /* frame coords (relative to rotation origin of this frame */
  Coord fx=x-a->x;
  Coord fy=y-a->y;
  Screencoord sx, sy;
  
  Actuator *sa;
  Coord wx, wy;

  {	/* intersect mouseline with z=0 plane in the graphframe coords */
    float alpha;
    Coord wx1, wy1, wz1, wx2, wy2, wz2;
    mapwfind(ad->vobj, fx, fy, &wx1, &wy1, &wz1, &wx2, &wy2, &wz2);
    alpha=wz2/(wz2-wz1);
    wx=INTERP(wx2, wx1, alpha);
    wy=INTERP(wy2, wy1, alpha);
  }

  gw->wx=wx;
  gw->wy=wy;
}

drawblip(x, y)
Coord x, y;
{
  rectf(x-PARTICLE_SIZE, y-PARTICLE_SIZE, x+PARTICLE_SIZE, y+PARTICLE_SIZE);
}

void
drawgraphics(a, panel)
     Actuator *a;
     Panel *panel;
{
  Viewframe *ad=(Viewframe *)a->data;
  Graphworld *gw=(Graphworld *)a->u;
  Particle *p;
  int i;

  Colorindex col;

  pushmatrix();
  translate(a->x, a->y, 0.0);
  callobj(ad->vobj);

  color(BLACK);
  if (smearbutton->val!=1.0) {
    frontbuffer(FALSE);
    rectf(0.0, 0.0, a->w, a->h);
  } else {
    frontbuffer(TRUE);
  }

  for (i=0,p=gw->pl;p&&i<nslider->val;i++,p=p->next) {
    pushmatrix();
    color(WHITE);
    drawblip(p->x, p->y);
    popmatrix();
  }

  color(YELLOW);
  if (a->active) 
    circf(RANGE(gw->wx, 0, a->w), RANGE(gw->wy, 0, a->h), 2*PARTICLE_SIZE);

  color(pnl_background_color);

  /* bottom */
  pmv2(-FRAMEMARGIN, -FRAMEMARGIN);
  pdr2(a->w+FRAMEMARGIN, -FRAMEMARGIN);
  pdr2(a->w, 0.0);
  pdr2(0.0, 0.0);
  pclos();
  
  /* left */
  pmv2(-FRAMEMARGIN, -FRAMEMARGIN);
  pdr2(-FRAMEMARGIN, a->h+FRAMEMARGIN);
  pdr2(0.0, a->h);
  pdr2(0.0, 0.0);
  pclos();

  /* top */
  pmv2(-FRAMEMARGIN, a->h+FRAMEMARGIN);
  pdr2(a->w+FRAMEMARGIN, a->h+FRAMEMARGIN);
  pdr2(a->w, a->h);
  pdr2(0.0, a->h);
  pclos();

  /* right */
  pmv2(a->w+FRAMEMARGIN, -FRAMEMARGIN);
  pdr2(a->w+FRAMEMARGIN, a->h+FRAMEMARGIN);
  pdr2(a->w, a->h);
  pdr2(a->w, 0.0);
  pclos();

  /* normally these are outside the popmatrix(), here we want them rotated */
  translate(-a->x, -a->y, 0.0);
  if (a->beveled) pnl_drawbevel(a, panel);
  if (a->label) pnl_drawlabel(a, panel);

  popmatrix();
}

void cleanexit() { exit(0); }

void newvaltoggleradiobutton(a,p,x,y)
Actuator *a;
Panel *p;
Coord x,y;
{
  Actuator *b;

  if (pnl_justdown) {
    if (a->val==a->maxval) a->val=a->minval;
    else {
      a->val=a->maxval;
      for (b=a->group;(b!=a);b=b->group) {
	b->val=b->minval;
	pnl_fixact(b);
      }
    }
    pnl_setdirty(a);
  }
}

void
grav_toggle_radio_button(a)
Actuator *a;
{
  pnl_radio_button(a);
  a->type+=PNL_USER_OFFSET+0;

  a->newvalfunc=newvaltoggleradiobutton;
}

defpanel()
{
  Actuator *a, *sa, *tb=pnl_mkact(pnl_toggle_button);
  Panel *p;
  Coord x, y, tx, ty, tw, th, d=1.0, dl=0.5;
  float sf;
  Viewframe *ad;

  p=pnl_mkpanel();
  p->label="viewframes";

  /* put this first so it will not interfere with picking the other */
  /* control elements */

  x=0;
  y=7;

  graphworld.a=graphframe=a=pnl_mkact(pnl_viewframe);
  a->label="graphics frame";
  a->x=x;
  a->y=y;
  a->w=20;
  a->h=8;
  a->newvalfunc=newvalgraphframe;
  a->drawfunc=drawgraphics;
  a->activefunc=attraction;
  a->u=(char *)&graphworld;
  ad=(Viewframe *)a->data;
  ad->mode|=PNL_FM_FIXED;
  pnl_addact(a, p);

  x=0;
  y=0;

  xgravityslider=a=pnl_mkact(pnl_slider);
  a->label="gx";
  a->minval= -1;
  a->x=x;
  a->y=y;
  pnl_addact(a, p);
  tw=a->w;
  tx=x+a->w+PNL_DIM_1;
  ty=y;
  y+=a->h+PNL_DIM_1;

  a=pnl_mkact(pnl_wide_button);
  a->label="R";
  a->x=x;
  a->y=y;
  a->w=tw;
  a->downfunc=resetgravity;
  a->u=(char *)xgravityslider;
  pnl_addact(a, p);
  y+=a->h+PNL_DIM_1;

  x=tx;
  y=ty;

  ygravityslider=a=pnl_mkact(pnl_slider);
  a->label="gy";
  a->val= -0.1;
  a->minval= -10.0;
  a->maxval= 10.0;
  a->x=x;
  a->y=y;
  pnl_addact(a, p);
  tw=a->w;
  tx=x+a->w+PNL_DIM_1;
  ty=y;
  y+=a->h+PNL_DIM_1;

  a=pnl_mkact(pnl_wide_button);
  a->label="R";
  a->x=x;
  a->y=y;
  a->w=tw;
  a->downfunc=resetgravity;
  a->u=(char *)ygravityslider;
  pnl_addact(a, p);
  y+=a->h+PNL_DIM_1;

  x=tx;
  y=ty;

  dragslider=a=pnl_mkact(pnl_slider);
  a->label="drag";
  a->x=x;
  a->y=y;
  pnl_addact(a, p);
  x+=a->w+PNL_DIM_1;

  overallattractionslider=a=pnl_mkact(pnl_slider);
  a->label="attr";
  a->x=x;
  a->y=y;
  pnl_addact(a, p);
  x+=a->w+PNL_DIM_1;

  nslider=a=pnl_mkact(pnl_slider);
  a->label="n";
  a->val=PARTICLES;
  a->maxval=PARTICLES;
  a->minval=1;
  a->x=x;
  a->y=y;
  pnl_addact(a, p);
  x+=a->w+PNL_DIM_1;

  a=pnl_mkact(pnl_wide_button);
  a->label="reset";
  a->x=x;
  a->y=y;
  a->downfunc=resetworldbyactuator;
  a->u=(char *)&graphworld;
  pnl_addact(a, p);
  y+=a->h+PNL_DIM_1;

  tx=x+a->w+PNL_DIM_1;

  a=pnl_mkact(pnl_wide_button);
  a->label="reverse";
  a->x=x;
  a->y=y;
  a->downfunc=reverse;
  a->u=(char *)&graphworld;
  pnl_addact(a, p);
  y+=a->h+PNL_DIM_1;

  repelbutton=a=pnl_mkact(pnl_toggle_button);
  a->label="repel";
  a->x=x;
  a->y=y;
  a->val= -1;
  a->minval= -1;
  a->maxval=1;
  pnl_addact(a, p);
  y+=a->h+PNL_DIM_1;

  wrapbutton=a=pnl_mkact(pnl_toggle_button);
  a->label="wrap";
  a->x=x;
  a->y=y;
  pnl_addact(a, p);
  y+=a->h+PNL_DIM_1;

  smearbutton=a=pnl_mkact(pnl_toggle_button);
  a->label="smear";
  a->x=x;
  a->y=y;
  pnl_addact(a, p);
  y+=a->h+PNL_DIM_1;

  x=tx+1;
  y=0;

  a=pnl_mkact(pnl_wide_button);
  a->label="exit";
  a->x=x;
  a->y=y;
  a->upfunc=cleanexit;
  pnl_addact(a, p);
  y+=a->h+PNL_DIM_1+dl;

  a=kestrip=pnl_mkact(pnl_scale_chart);
  a->label="kinetic energy";
  a->w+=2;
  a->x=x;
  a->y=y;
  pnl_addact(a, p);
  y+=a->h+PNL_DIM_1+dl;

  a=pestrip=pnl_mkact(pnl_scale_chart);
  a->label="potential energy";
  a->w+=2;
  a->x=x;
  a->y=y;
  pnl_addact(a, p);
  y+=a->h+PNL_DIM_1+dl;

  x+=a->w+PNL_DIM_1+1;
  y=0;
}

