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

Actuator *puck, *rpuck, *fpuck;
Actuator *sr, *hs;
Actuator *d1, *d2, *d3, *pal1, *pal2, *palout;
Actuator *ti, *to;
Actuator *dvs, *m1, *m2, *st1, *st2;
Actuator *ta;

float savedx=0.0, savedy=0.0;

Panel
*defpanel();

main() 
{
  Actuator *a;
  Panel *panel;
  static  intext = 0;
  
  foreground();
  noport();
  winopen("demo");
  
  doublebuffer();
  gconfig();
  
  ortho2(-1.0,1.0,-1.0,1.0);
  
  panel=defpanel();
  
  setcolors();

  for (;;) {
    a=pnl_dopanel();
    if (a==ti) {
      intext = 1;
    }
    else if (intext)
    {
	intext = 0;
        tprint(to, PNL_ACCESS(Typein, ti, str));
        *(PNL_ACCESS(Typein, ti, str)) = 0;
	pnl_fixact (ti);
    }
    if (a==dvs) {
      m1->val=m2->val=st1->val=st2->val=dvs->val;
      pnl_fixact(m1);
      pnl_fixact(m2);
      pnl_fixact(st1);
      pnl_fixact(st2);
    }
    if (a==sr) {
      hs->val=sr->val;
      pnl_fixact(hs);
    }
    if (a==hs) {
      sr->val=hs->val;
      pnl_fixact(sr);
    }
    if (a==rpuck||a==fpuck) {
      PNL_ACCESS(Puck, puck, x)=savedx+PNL_ACCESS(Puck, a, x);
      PNL_ACCESS(Puck, puck, y)=savedy+PNL_ACCESS(Puck, a, y);
      pnl_fixact(puck);
      pnl_delact(ta);
    }
    swapbuffers();
  }
}

setcolors()
{
}

void
updatecolor(a)
     Actuator *a;
{
  mapcolor((short)palout->val, (short) (255*d1->val),
	   		       (short) (255*d2->val),
	   		       (short) (255*d3->val));
}

void
pickcolor(a)
     Actuator *a;
{
  short r, g, b;

  getmcolor((short)a->val, &r, &g, &b);
  d1->val=r/256.0;
  d2->val=g/256.0;
  d3->val=b/256.0;
  palout->minval=palout->maxval=palout->val=a->val;
  pnl_fixact(d1);
  pnl_fixact(d2);
  pnl_fixact(d3);
  pnl_fixact(palout);
}

void cleanexit() {exit(0);}

void savepos(a) 
     Actuator *a;
{
  savedx=PNL_ACCESS(Puck, puck, x);
  savedy=PNL_ACCESS(Puck, puck, y);
}

#define MKACT(_a, _type,_label)	\
  a=(_a)=pnl_mkact(_type);		\
  (_a)->label=_label

#define ADDACT \
  a->x=x;			\
  a->y=(y-=a->h+dl+PNL_DIM_1);	\
  pnl_addact(a, p)

#define OVER	\
  x+=a->w+PNL_DIM_1;	\
  y+=a->h+dl+PNL_DIM_1

Panel
*defpanel()
{
  Actuator *a;
  Panel *p;
  Coord y, x, d=1.0, dl;

  initscriptpanel();

  p=pnl_mkpanel();
  p->label="mondo";

/* the buttons */

  dl= -PNL_DIM_1/2.0;
  x=0;
  y=0+dl;

  MKACT(a, pnl_button, "button");		ADDACT;
  MKACT(a, pnl_toggle_button, "toggle button");		ADDACT;

  y-=0.5;

  MKACT(a, pnl_radio_button, "radio button 1");		ADDACT;
  MKACT(a, pnl_radio_button, "radio button 2");		ADDACT;
  MKACT(a, pnl_radio_button, "radio button 3");		ADDACT;
ta=a;
  pnl_endgroup(p);

  y-=0.5;

  MKACT(a, pnl_wide_button, "wide");			ADDACT;
  MKACT(a, pnl_wide_button, "exit");
  a->upfunc=cleanexit;					ADDACT;

/* the arrow buttons */

  dl= -PNL_DIM_1/2.0;
  x=5;
  y=0+dl;

  MKACT(a, pnl_up_double_arrow_button, NULL);		ADDACT;
  MKACT(a, pnl_up_arrow_button, NULL);			ADDACT;
  x-=1;
  MKACT(a, pnl_left_double_arrow_button, NULL);		ADDACT;
  OVER; x-=PNL_DIM_2;
  MKACT(a, pnl_left_arrow_button, NULL);		ADDACT;
  OVER; x-=PNL_DIM_2;
  MKACT(a, pnl_button, NULL);				ADDACT;
  OVER; x-=PNL_DIM_2;
  MKACT(a, pnl_right_arrow_button, NULL);		ADDACT;
  OVER; x-=PNL_DIM_2;
  MKACT(a, pnl_right_double_arrow_button, NULL);	ADDACT;
  x-=1;
  MKACT(a, pnl_down_arrow_button, NULL);		ADDACT;
  MKACT(a, pnl_down_double_arrow_button, "arrow buttons");	ADDACT;

/* the pucks */

  dl=0;
  x=0;
  y= -5.5;

  MKACT(puck, pnl_puck, "puck");			ADDACT;
  OVER;
  y-=1;
  MKACT(fpuck, pnl_floating_puck, "floating puck");
  a->labeltype=PNL_LABEL_RIGHT;			ADDACT;
  a->upfunc=savepos;
  MKACT(rpuck, pnl_rubber_puck, "rubber puck");
  a->upfunc=savepos;
  a->labeltype=PNL_LABEL_RIGHT;			ADDACT;

/* the slider and roid */

  dl=0.5;
  x=7.5;
  y= -1.0+dl;

  x+=2;
  MKACT(sr, pnl_slideroid, "slideroid");		ADDACT;
  x-=2;
  MKACT(hs, pnl_hslider, "horizontal slider");	ADDACT;

/* the palettes and dials */  

  dl=0;
  x=8;
  y= -5;

  MKACT(pal1, pnl_vpalette, NULL);
  a->h=5-PNL_DIM_1;
  a->activefunc=pickcolor;
  a->minval=128; a->maxval=511;		ADDACT;
  OVER;
  MKACT(d1, pnl_dial, "R");
  a->activefunc=updatecolor;
  a->labeltype=PNL_LABEL_TOP;		ADDACT;
  OVER;
  MKACT(d2, pnl_dial, "G");
  a->activefunc=updatecolor;
  a->labeltype=PNL_LABEL_TOP;		ADDACT;
  OVER;
  MKACT(d3, pnl_dial, "B");
  a->activefunc=updatecolor;
  a->labeltype=PNL_LABEL_TOP;		ADDACT;
  OVER;

  x=8;
  y= -5;

  x+=1;
  y-=1;

  MKACT(palout, pnl_vpalette, NULL);
  a->minval=a->val=a->maxval=64;
  a->h=3-PNL_DIM_1; a->w=3-PNL_DIM_1;			ADDACT;
  MKACT(pal2, pnl_hpalette, "palette");
  a->activefunc=pickcolor;
  a->minval=8; a->maxval=30;
  a->w=3-PNL_DIM_1;					ADDACT;

/* the typein/outs */

  dl=0.5;
  x=14;
  y=0;

  MKACT(to, pnl_typeout, "typeout");
  a->labeltype=PNL_LABEL_TOP;
  PNL_ACCESS(Typeout,a,lin)=10;
  a->y=(y-=3);						ADDACT;
  MKACT(ti, pnl_typein, "typein");			ADDACT;
  
/* the meters and strip charts */

  dl=0.5;
  x=13;
  y= -5;

  MKACT(dvs, pnl_dvslider, NULL);
  a->h=4-PNL_DIM_1+dl;					ADDACT;
  OVER;
  MKACT(m1, pnl_analog_meter, "meter");			ADDACT;
  MKACT(m2, pnl_analog_bar, "bar meter");		ADDACT;
  OVER;
  y= -5;
  MKACT(st1, pnl_strip_chart, "strip chart");		ADDACT;
  MKACT(st2, pnl_scale_chart, "scaling strip chart");	ADDACT;

  return p;
}

