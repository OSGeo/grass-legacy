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
/*
 *	This software is in the public domain, it may not be resold
 *	or relicensed.  Modified and enhanced versions of this software
 *	are likewise to be made freely available.  Sites using this
 *	software are requested to register with NASA at the address below.  
 *	Send modifications and requests for most recent version to:
 *
 *	Author:  David A. Tristram
 *		 ATTN: Panel Library
 *		 MS 258-5
 *		 NASA  Ames Research Center
 *		 Moffett Field, CA  94035
 *
 *		 415-694-4404
 *		 dat@orville.nas.nasa.gov
 */
#include <math.h>
#include <gl.h>
#include <device.h>
#include <panel.h>

#define MAXCOLORS	4096	/* for simplicity just make the storage */
				/* the size of the whole map */
#define CONTROLBASE	512
#define BWRAMP	        CONTROLBASE+0
#define BWRAMPSIZE	128
#define SPECTRUM	BWRAMP+BWRAMPSIZE
#define SPECTRUMSIZE	64
#define SATRAMP		SPECTRUM+SPECTRUMSIZE
#define SATRAMPSIZE	64
#define HUECHANINDEX	SATRAMP+SATRAMPSIZE

#define NCOLORS		128
#define BASECOLOR	256
#define HUE_RATE_FACTOR 0.05;	/* how fast through hue space */
#define HUECHANNELS	6	/* number of huecontrols in colors panel */

#define INTERP_MODE	0
#define CHASE_MODE	1
#define INTERP_RGB	0.0
#define INTERP_HLS	1.0

#define INTERP(x1, x2, a) ((a)*(x2)+(1.0-(a))*(x1))

int animationmode = CHASE_MODE;

int n;	    /* what one are we drawing now? */
float t;    /* how far are we toward the next color (1 always for now) */
int ncolors;/* how many colors do we get? */
int basecolor;/* lowest color that we use */

float *hue, *light, *sat;
float gsat, glight, ghue, bhue;
float gchant;			/* channel selction parameter */
int gchan;			/* current channel */

Alist *modsliderlist;
Panel *panel;

Actuator *hslider[HUECHANNELS], *lslider[HUECHANNELS], *sslider[HUECHANNELS];
Actuator *activebutton[HUECHANNELS], *dwellslider[HUECHANNELS];
Actuator *chanrateslider;
Actuator *huepalette, *lightpalette, *satpalette, *hueindex[HUECHANNELS];
Actuator *lowcolortypein, *highcolortypein, *stopbutton;
Actuator *mappalette;
Actuator *chanpositionrateslider[HUECHANNELS], *chanpositionmslider;
Actuator *chanpositionpalette[HUECHANNELS];
Actuator *speedslider, *interpmodebutton;

int positionsense[HUECHANNELS];

#define WRAP	0
#define BOUNCE	1

#define MSLIDERVAL(Mname) (((Modslider *)(Mname/**/slider->u))->valbar->extval)
typedef struct {
  int mode;	/* WRAP or BOUNCE */
  float center, range, sense;
  Actuator *ulbar, *valbar, *llbar, *freqslider;
  Actuator *modebutton;
} Modslider;

struct resetbutton {
    Actuator button;
    Actuator *targetact;
    float    *targetvar;
    float    varinitval;
};

void
resetact(b)
struct resetbutton *b;
{
    if (b->targetact->val!=b->targetact->initval) {
	b->targetact->val=b->targetact->initval;
        fixact(b->targetact);
    } else if (b->targetvar) {
	*b->targetvar=b->varinitval;
    }
}

void
resetslider(b)
struct resetbutton *b;
{
  Modslider *au=(Modslider *)b->targetact->u;

  au->ulbar->extval=au->valbar->pa->maxval;
  au->llbar->extval=au->valbar->pa->minval;
  au->freqslider->val=0;
  fixact(au->freqslider);
  fixact(au->valbar->pa);

  if (au->valbar->extval!=au->valbar->pa->initval) {
    au->valbar->extval=b->targetact->initval;
    fixact(b->targetact);
  } else if (b->targetvar) {
    *b->targetvar=b->varinitval;
  }
}

void saverange(a)
Actuator *a;
{
  Modslider *au=(Modslider *)a->pa->u;

  au->range=au->ulbar->extval-au->llbar->extval;
  au->center=au->ulbar->extval-au->range/2.0;
}

void setcenter(a)
Actuator *a;
{
  Modslider *au=(Modslider *)a->pa->u;

  au->ulbar->extval=au->llbar->extval+au->range;
  au->ulbar->extval=RANGE(au->ulbar->extval, au->llbar->extval, a->pa->maxval);
  au->valbar->extval=RANGE(au->valbar->extval,
			   au->llbar->extval,
			   au->ulbar->extval);
  saverange(a);
  fixact(a->pa);
}

void setrange(a)
Actuator *a;
{
  Modslider *au=(Modslider *)a->pa->u;

  au->ulbar->extval=2*au->center-au->llbar->extval;
  au->ulbar->extval=RANGE(au->ulbar->extval, au->llbar->extval, a->pa->maxval);
  au->valbar->extval=RANGE(au->valbar->extval,
			   au->llbar->extval,
			   au->ulbar->extval);
  saverange(a);
  fixact(a->pa);
}

static short savR[MAXCOLORS], savG[MAXCOLORS], savB[MAXCOLORS];

void
load_spectrum_map(basecolor, ncolors)
int basecolor, ncolors;
{
float h, s, v, r, g, b, x;
int i, ir, ig, ib;

  x = 0.0;
  for (i=0; i<ncolors; i++)
    {
      x = (float)i / (float)ncolors;
      hls_to_rgb (x, 0.5, 1.0, &r, &g, &b);
      rgb_to_irgb (r, g, b, &ir, &ig, &ib);
      mapcolor ((Colorindex)(basecolor+i), ir, ig, ib);
    }
}

void
load_bw_map(basecolor, ncolors)
int basecolor, ncolors;
{
float h, s, v, r, g, b, x;
int i, ir, ig, ib;

  x = 0.0;
  for (i=0; i<ncolors; i++)
    {
      x = (float)i / (float)ncolors;
      hls_to_rgb (1.0, x, 0.0, &r, &g, &b);
      rgb_to_irgb (r, g, b, &ir, &ig, &ib);
      mapcolor ((Colorindex)(basecolor+i), ir, ig, ib);
    }
}

void
load_sat_map(basecolor, ncolors)
int basecolor, ncolors;
{
float h, s, v, r, g, b, x;
int i, ir, ig, ib;

  x = 0.0;
  for (i=0; i<ncolors; i++)
    {
      x = (float)i / (float)ncolors;
      hls_to_rgb (1.0, 0.5, x, &r, &g, &b);
      rgb_to_irgb (r, g, b, &ir, &ig, &ib);
      mapcolor ((Colorindex)(basecolor+i), ir, ig, ib);
    }
}

void
savecolors()
{
int i;

  for (i=0;i<ncolors;i++)
    getmcolor(i+basecolor, &savR[i], &savG[i], &savB[i]);
}

void
restorecolors()
{
int i;

  for (i=0;i<ncolors;i++)
    mapcolor(i+basecolor, savR[i], savG[i], savB[i]);
}

void
loadmap()
{
  int i, j;
  int ir, ig, ib;
  float r, g, b;

  for (i=0, j=n;i<ncolors;i++, j--) {
    if (j<0) j+=ncolors;
    hls_to_rgb(hue[j], light[j], sat[j], &r, &g, &b);
    rgb_to_irgb (r, g, b, &ir, &ig, &ib);
    mapcolor(i+basecolor, ir, ig, ib);
  }
}

interpmap()
{
  int i, chan;
  float thue1, thue2, thue;
  float tlight1, tlight2, tlight;
  float tsat1, tsat2, tsat;

  float a, x, x1, x2;

  int ir, ig, ib;
  float r1, g1, b1;
  float r2, g2, b2;
  float r, g, b;

  for (i=0; i<ncolors; i++) {
    x=i/(float)ncolors;
    for (chan=0;chan<HUECHANNELS;chan++) {
      if (x<chanpositionpalette[chan]->extval) break;
    }
    if (chan==0) {
      thue1=((Modslider *)hslider[HUECHANNELS-1]->u)->valbar->extval;
      thue2=((Modslider *)hslider[0]->u)->valbar->extval;
      tlight1=((Modslider *)lslider[HUECHANNELS-1]->u)->valbar->extval;
      tlight2=((Modslider *)lslider[0]->u)->valbar->extval;
      tsat1=((Modslider *)sslider[HUECHANNELS-1]->u)->valbar->extval;
      tsat2=((Modslider *)sslider[0]->u)->valbar->extval;
      
      x1=chanpositionpalette[HUECHANNELS-1]->extval-1.0;
      x2=chanpositionpalette[0]->extval;

    } else if (chan>HUECHANNELS-1) {

      chan--;
      thue1=((Modslider *)hslider[chan]->u)->valbar->extval;
      thue2=((Modslider *)hslider[0]->u)->valbar->extval;
      tlight1=((Modslider *)lslider[chan]->u)->valbar->extval;
      tlight2=((Modslider *)lslider[0]->u)->valbar->extval;
      tsat1=((Modslider *)sslider[chan]->u)->valbar->extval;
      tsat2=((Modslider *)sslider[0]->u)->valbar->extval;
      
      x1=chanpositionpalette[chan]->extval;
      x2=1.0+chanpositionpalette[0]->extval;

    } else {

      chan--;
      thue1=((Modslider *)hslider[chan]->u)->valbar->extval;
      thue2=((Modslider *)hslider[chan+1]->u)->valbar->extval;
      tlight1=((Modslider *)lslider[chan]->u)->valbar->extval;
      tlight2=((Modslider *)lslider[chan+1]->u)->valbar->extval;
      tsat1=((Modslider *)sslider[chan]->u)->valbar->extval;
      tsat2=((Modslider *)sslider[chan+1]->u)->valbar->extval;
      
      x1=chanpositionpalette[chan]->extval;
      x2=chanpositionpalette[chan+1]->extval;
    }

    a=(x-x1)/(x2-x1);
    if (interpmodebutton->val==INTERP_RGB) {
      thue1=fmod(thue1, 1.0);
      thue2=fmod(thue2, 1.0);
      hls_to_rgb(thue1, tlight1, tsat1, &r1, &g1, &b1);
      hls_to_rgb(thue2, tlight2, tsat2, &r2, &g2, &b2);
      r=INTERP(r1, r2, a);
      g=INTERP(g1, g2, a);
      b=INTERP(b1, b2, a);
    } else {
      thue = a*thue2+(1.0-a)*thue1;
      thue=fmod(thue, 1.0);
      tlight = a*tlight2+(1.0-a)*tlight1;
      tsat = a*tsat2+(1.0-a)*tsat1;
      
#if 0
      printf("chan=%d, i=%d, a=%f, thue=%f, tlight=%f, tsat=%f\n",
	     chan, i, a, thue, tlight, tsat); 
#endif DEBUG
      
      hls_to_rgb(thue, tlight, tsat, &r, &g, &b);
    }
    rgb_to_irgb (r, g, b, &ir, &ig, &ib);
    mapcolor(i+basecolor, ir, ig, ib);
  }
}


void
cleanexit()
{
  restorecolors();
  exit(0);
}

animateslider(a)
     Actuator *a;
{
  Modslider *au=(Modslider *)a->u;
  float dx=au->sense*speedslider->val*
    au->freqslider->val*
      (au->ulbar->extval-au->llbar->extval)/(a->maxval-a->minval);
  
  if (pnl_ca==au->valbar) return;
  if (au->freqslider->val==0.0) return;
  
  au->valbar->extval+=dx;
  
  if (au->modebutton->val == BOUNCE) {
    if (au->valbar->extval<au->llbar->extval) {
      au->valbar->extval= -au->valbar->extval+2*au->llbar->extval;
      au->sense=1;
    } else if (au->valbar->extval>au->ulbar->extval) {
      au->valbar->extval= -au->valbar->extval+2*au->ulbar->extval;
      au->sense= -1;
    }
  } else {
    if (au->valbar->extval<au->llbar->extval) {
      au->valbar->extval =
	au->ulbar->extval - (au->llbar->extval - au->valbar->extval);
    } else if (au->valbar->extval>au->ulbar->extval) {
      au->valbar->extval =
	au->llbar->extval + (au->valbar->extval - au->ulbar->extval);
    }
  }
  au->valbar->extval=RANGE(au->valbar->extval,
			   au->llbar->extval,
			   au->ulbar->extval);
  a->val=au->valbar->extval;
  fixact(a);
}

animatesliders()
{
  Alist *e;

  for (e=modsliderlist;e;e=e->next) animateslider(e->a);
}


animatepositionslider()
{
  float dx, ul, ll;
  int chan;

  for (chan=0;chan<HUECHANNELS;chan++) {
    if (chanpositionrateslider[chan]->val==0.0) continue;
    if (chan==0) {
      ll=chanpositionmslider->minval;
      ul=chanpositionpalette[chan+1]->extval;
    } else if (chan==HUECHANNELS-1) {
      ll=chanpositionpalette[chan-1]->extval;
      ul=chanpositionmslider->maxval;
    } else {
      ll=chanpositionpalette[chan-1]->extval;
      ul=chanpositionpalette[chan+1]->extval;
    }
    dx=positionsense[chan]*speedslider->val*
      chanpositionrateslider[chan]->val*(ul-ll);
    chanpositionpalette[chan]->extval+=dx;
    if (chanpositionpalette[chan]->extval<ll) {
      chanpositionpalette[chan]->extval= -chanpositionpalette[chan]->extval
	+ 2*ll;
      positionsense[chan]=1;
    } else if (chanpositionpalette[chan]->extval>ul) {
      chanpositionpalette[chan]->extval= -chanpositionpalette[chan]->extval
	+ 2*ul;
      positionsense[chan]= -1;
    }
    /*  fixact(chanpositionpalette[chan]); */
  }
  fixact(chanpositionmslider);
}
	
	
	

main()
{
  Actuator *a;
  Device dev;
  short val;
  int h, i, j, mexwin;
  int itmp;
  float tmp;
  Boolean newn;

  n=0;
  hue=(float *)alloc(MAXCOLORS*sizeof(float));
  light=(float *)alloc(MAXCOLORS*sizeof(float));
  sat=(float *)alloc(MAXCOLORS*sizeof(float));
  for (i=0;i<HUECHANNELS;i++) positionsense[i]=1;
 
#ifdef DEBUG
  foreground();
#endif DEBUG
  noport();
  winopen("cam");

  switch (getplanes()) {
  case 4:
    ncolors=15;
    basecolor=1;

    pnl_white_color		= 4;
    pnl_bevel_light_color	= 13;
    pnl_normal_color		= 9;
    pnl_other_color		= 7;
    pnl_background_color	= 8;
    pnl_highlight_color		= 5;
    pnl_bevel_dark_color	= 2;
    pnl_label_color		= 12;
    pnl_black_color		= 0;

    break;
  case 8:
    ncolors=128;
    basecolor=128;
    break;
  case 12:
  default:
    ncolors=128;
    basecolor=BASECOLOR;
    break;
  }
  savecolors();
  load_spectrum_map(SPECTRUM, SPECTRUMSIZE);
  load_bw_map(BWRAMP, BWRAMPSIZE);
  load_sat_map(SATRAMP, SATRAMPSIZE);

  defpanel();

#ifdef IRIS_GT
  cmode();
#endif IRIS_GT
  gconfig();

  gchan=0;
  gchant=0;
  ghue=0.5;

  for (;;) {

    a=dopanel();

    if (stopbutton->val==1.0) {
      swapbuffers();
      do {
	a=dopanel();
	if (!a) sginap(0);
	swapbuffers();
      } while (stopbutton->val==1.0);
    }

    t+=1/* * speedslider->val */;  /* uncomment this for real speed control */
    n+=floor(t);
    t=fmod(t,1.0);

    if (n>=ncolors) n-=ncolors;
    
    gchant+=chanrateslider->val*speedslider->val;
    gchant=fmod(gchant,1.0);

    ghue=((Modslider *)hslider[gchan]->u)->valbar->extval;
    glight=((Modslider *)lslider[gchan]->u)->valbar->extval;
    gsat=((Modslider *)sslider[gchan]->u)->valbar->extval;

    while(ghue<0) ghue+=1;
    while(ghue>=1) ghue-=1;
    while(bhue<0) bhue+=1;
    while(bhue>=1) bhue-=1;

    hue[n%ncolors]=ghue;
    light[n%ncolors]=glight;
    sat[n%ncolors]=gsat;

    { int i;
      float hue, light, sat, tot=0.0;
      int ir, ig, ib;
      float r,g,b;
      Boolean holding;

      holding=FALSE;
      for (i=0;i<HUECHANNELS;i++)
	if (pnl_ca==activebutton[i]) {  /* hold on one channel */
	  holding=TRUE;
	  gchan=i;
	  break;
	}

      if (!holding) {
	for (i=0;i<HUECHANNELS;i++) {
	  gchan=0;
	  tot+=dwellslider[i]->val;
	  if (tot>gchant) {
	    gchan=i;
	    break;
	  }
	}
	
	if (activebutton[gchan]->val!=1.0) {
	  for (i=0;i<HUECHANNELS;i++) activebutton[i]->val=0.0;
	  activebutton[gchan]->val=1.0;
	  for (i=0;i<HUECHANNELS;i++) fixact(activebutton[i]);
	}
      }
      
      /* for the color indicator palettes */
      for (i=0;i<HUECHANNELS;i++) {
	hue=((Modslider *)hslider[i]->u)->valbar->extval;
	hue=fmod(hue, 1.0);
	light=((Modslider *)lslider[i]->u)->valbar->extval;
	sat=((Modslider *)sslider[i]->u)->valbar->extval;
	hls_to_rgb(hue, light, sat, &r, &g, &b);
	rgb_to_irgb (r, g, b, &ir, &ig, &ib);
	mapcolor(HUECHANINDEX+i, ir, ig, ib);
      }
    }

    animatesliders();
    animatepositionslider();
    if (animationmode)
      loadmap();
    else
      interpmap();

#ifndef IRIS_4D
    swapbuffers();
#endif  IRIS_4D
  }
}

float
twixt(i,x,t)
int i;
float *x;
float t;
{
int j=i-1;

    if (j<0) j+=ncolors;
    return x[i]*t+x[j]*(1.0-t);
}

float
foldtwixt(i,x,t, range)
int i;
float *x;
float t, range;
{
  float r;
  float d;
  int j=i-1;

    if (j<0) j+=ncolors;
    d=x[i]-x[j];
    if	    (d> range/2.0) r=x[i]*t+(x[j]+range)*(1.0-t);
    else if (d<-range/2.0) r=(x[i]+range)*t+x[j]*(1.0-t);
    else	       r=x[i]*t+x[j]*(1.0-t);

    return r;
}

Actuator
*mkepslider(label, x, y, min, max, val, var, mode, panel)
  char *label;
  Coord x, y;
  float min, max, val, *var;
  int mode;
  Panel *panel;
{
  Alist *e;
  Actuator *a, *sa;
  Modslider *au;
  struct resetbutton *rb;

    a=mkact(pnl_multislider);
    PNL_ACCESS(Multislider, a, mode)=PNL_MSM_ORDERED;
    PNL_ACCESS(Multislider, a, n)=0;
    a->label=label;
    a->x=x;
    a->y=y;
    a->val=val;
    a->minval=min;
    a->maxval=max;
    au=(Modslider *)(a->u=(char *)alloc(sizeof(Modslider)));
    au->sense=1;
    addact(a, panel);

    au->ulbar=sa=mkact(pnl_multislider_open_bar);
    sa->extval= a->maxval;
    sa->downfunc=saverange;
    addsubact(sa, a);

    au->llbar=sa=mkact(pnl_multislider_open_bar);
    sa->extval= a->minval;
    sa->downfunc=saverange;
    sa->activefunc=setcenter;
    addsubact(sa, a);

/* this appears third so it will fall between the other two even if it */
/* is at the end of a slider (that is a property of multisliders) */

    au->valbar=sa=mkact(pnl_multislider_bar);
    sa->extval=val;
    addsubact(sa, a);

    au->freqslider=sa=mkact(pnl_hslider);
    sa->w=a->w;
    sa->h=2*PNL_ACCESS(Multislider, a, bh);
    sa->x=a->x;
    sa->y=a->y+a->h+PNL_DIM_2;
    sa->maxval=(a->maxval-a->minval)/2.0;
    PNL_ACCESS(Slider, sa, mode)|=PNL_SM_NOSNAP;
    sa->downfunc=saverange;
    sa->activefunc=setrange;
    addact(sa, panel);
    sa->pa=a;

    au->modebutton=sa=mkact(pnl_toggle_button);
    sa->w=a->w;
    sa->h=2*PNL_ACCESS(Multislider, a, bh);
    sa->x=a->x;
    sa->y=a->y+a->h+au->freqslider->h+2*PNL_DIM_2;
    sa->val=mode;
    sa->minval=BOUNCE;
    sa->maxval=WRAP;
    addact(sa, panel);

    rb=PNL_MKUSERACT(resetbutton, pnl_wide_button);
    rb->targetact=a;
    rb->targetvar=var;
    rb->varinitval=val;
    rb->button.label="R";
    rb->button.w=PNL_SLIDER_WIDTH;
    rb->button.h=PNL_BUTTON_EDGE;
    rb->button.x=x;
    rb->button.y=sa->y+sa->h+PNL_DIM_2;
    rb->button.downfunc=resetslider;
    addact((Actuator *)rb, panel);

    e=(Alist *)alloc(sizeof(Alist));
    e->a=a;
    e->next=modsliderlist;
    modsliderlist=e;

    return a;
}

#ifdef 0
updatedwellsliders(i)
int i;
{
  int j,k;
  float tot;

  for (j=0;j<HUECHANNELS;j++) {
    tot=0.0;
    if (j==i) continue;
    for (k=0;k<HUECHANNELS;k++) {
      if (k==j) continue;
      tot+=dwellslider[k]->val;
    }
    dwellslider[j]->val=RANGE(1.0-tot, 0.0, 1.0);
  }
  for (j=0;j<HUECHANNELS;j++) fixact(dwellslider[j]);
}
#endif

updatedwellsliders(i)
int i;
{
  int j,k;
  float tot;
  static float oldval[3];

  for (j=0, tot=0;j<HUECHANNELS;j++) {
    if (j==i) continue;
    tot+=dwellslider[j]->val;
  }

  for (j=0;j<HUECHANNELS;j++) {
    if (j==i) continue;
    if (tot==0.0) 
      dwellslider[j]->val+=
	-(dwellslider[i]->val-oldval[i])*(1.0/(float)HUECHANNELS);
    else
      dwellslider[j]->val+=
	-(dwellslider[i]->val-oldval[i])*dwellslider[j]->val/tot;
    
    dwellslider[j]->val=RANGE(dwellslider[j]->val, 0, 1);
  }

  for (j=0;j<HUECHANNELS;j++) fixact(dwellslider[j]);

  for (j=0;j<HUECHANNELS;j++)
    oldval[j]=dwellslider[j]->val;  
}

updateactivebutton(i)
int i;
{
  int j;

  for (j=0;j<HUECHANNELS;j++) {
    if (j==i) continue;
    activebutton[j]->val=activebutton[j]->minval;
    fixact(activebutton[j]);
  }
}

void
togglechanmode(a)
Actuator *a;
{
  int i;

  if (a->u[0]=='h') {
    fixact(a);
    huepalette->visible=FALSE;
    lightpalette->visible=TRUE;
    fixact(huepalette);
    fixact(lightpalette);
    for (i=0;i<HUECHANNELS;i++) {
      hslider[i]->visible=FALSE;
      ((Modslider *)(hslider[i]->u))->freqslider->visible=FALSE;
      ((Modslider *)(hslider[i]->u))->modebutton->visible=FALSE;
      lslider[i]->visible=TRUE;
      ((Modslider *)(lslider[i]->u))->freqslider->visible=TRUE;
      ((Modslider *)(lslider[i]->u))->modebutton->visible=TRUE;
      fixact(hslider[i]);
      fixact(((Modslider *)(hslider[i]->u))->freqslider);
      fixact(((Modslider *)(hslider[i]->u))->modebutton);
      fixact(lslider[i]);
      fixact(((Modslider *)(lslider[i]->u))->freqslider);
      fixact(((Modslider *)(lslider[i]->u))->modebutton);
    }
  } else if (a->u[0]=='l') {
    fixact(a);
    lightpalette->visible=FALSE;
    satpalette->visible=TRUE;
    fixact(lightpalette);
    fixact(satpalette);
    for (i=0;i<HUECHANNELS;i++) {
      lslider[i]->visible=FALSE;
      ((Modslider *)(lslider[i]->u))->freqslider->visible=FALSE;
      ((Modslider *)(lslider[i]->u))->modebutton->visible=FALSE;
      sslider[i]->visible=TRUE;
      ((Modslider *)(sslider[i]->u))->freqslider->visible=TRUE;
      ((Modslider *)(sslider[i]->u))->modebutton->visible=TRUE;
      fixact(lslider[i]);
      fixact(((Modslider *)(lslider[i]->u))->freqslider);
      fixact(((Modslider *)(lslider[i]->u))->modebutton);
      fixact(sslider[i]);
      fixact(((Modslider *)(sslider[i]->u))->freqslider);
      fixact(((Modslider *)(sslider[i]->u))->modebutton);
    }
  } else {
    fixact(a);
    huepalette->visible=TRUE;
    satpalette->visible=FALSE;
    fixact(huepalette);
    fixact(satpalette);
    for (i=0;i<HUECHANNELS;i++) {
      hslider[i]->visible=TRUE;
      ((Modslider *)(hslider[i]->u))->freqslider->visible=TRUE;
      ((Modslider *)(hslider[i]->u))->modebutton->visible=TRUE;
      sslider[i]->visible=FALSE;
      ((Modslider *)(sslider[i]->u))->freqslider->visible=FALSE;
      ((Modslider *)(sslider[i]->u))->modebutton->visible=FALSE;
      fixact(hslider[i]);
      fixact(((Modslider *)(hslider[i]->u))->freqslider);
      fixact(((Modslider *)(hslider[i]->u))->modebutton);
      fixact(sslider[i]);
      fixact(((Modslider *)(sslider[i]->u))->freqslider);
      fixact(((Modslider *)(sslider[i]->u))->modebutton);
    }
  }
}      


void
toggleanimationmode(a)
Actuator *a;
{
  int i;

  if (animationmode==INTERP_MODE) {
    animationmode=CHASE_MODE;
    for (i=0;i<HUECHANNELS;i++) {
      dwellslider[i]->visible=TRUE;
      activebutton[i]->visible=TRUE;
      chanpositionrateslider[i]->visible=FALSE;
    }
    chanrateslider->visible=TRUE;
    chanpositionmslider->visible=FALSE;
    interpmodebutton->visible=FALSE;
  } else {
    animationmode=INTERP_MODE;
    for (i=0;i<HUECHANNELS;i++) {
      dwellslider[i]->visible=FALSE;
      activebutton[i]->visible=FALSE;
      chanpositionrateslider[i]->visible=TRUE;
    }
    chanrateslider->visible=FALSE;
    chanpositionmslider->visible=TRUE;
    interpmodebutton->visible=TRUE;
  }
  fixpanel(a->p);
}

void
setcolorlimits(a)
Actuator *a;
{
  int highcolor;
  Typein *ad;

  ad=(Typein *)lowcolortypein->data;
  printf("low: %s ", ad->str);
  sscanf(ad->str, "%d", &basecolor);
  ad=(Typein *)highcolortypein->data;
  printf("high: %s\n", ad->str);
  sscanf(ad->str, "%d", &highcolor);

  mappalette->minval=basecolor;
  mappalette->maxval=highcolor;
  fixact(mappalette);
  ncolors=highcolor-basecolor;
}

void
constraincolorpanel(p)
Panel *p;
{
  int i;

  for (i=0;i<HUECHANNELS;i++) {
    if (pnl_ca==dwellslider[i]) updatedwellsliders(i);
    if (pnl_ca==activebutton[i]) updateactivebutton(i);
  }
}

Coord
  *pack2f(x, y)
Coord x, y;
{
  static Coord v[2];

  *v=x;
  *(v+1)=y;
  return v;
}

void
drawdoublepalette(a, p)
     Actuator *a;
     Panel *p;
{
  if (!a->dirtycnt) return;

  pushmatrix();
  
  translate(a->x,a->y,0.0);
  
#ifdef IRIS_4D
  bgnpolygon();
  color((Colorindex)a->minval);
  v2f(pack2f(0.0, 0.0));
  v2f(pack2f(a->w, 0.0));
  color((Colorindex)a->maxval);
  v2f(pack2f(a->w, a->h/2.0));
  v2f(pack2f(0.0,  a->h/2.0));
  endpolygon();

  bgnpolygon();
  color((Colorindex)a->minval);
  v2f(pack2f(0.0, a->h/2.0));
  v2f(pack2f(a->w, a->h/2.0));
  color((Colorindex)a->maxval);
  v2f(pack2f(a->w, a->h));
  v2f(pack2f(0.0,  a->h));
  endpolygon();
#else IRIS_4D
  setshade((Colorindex)a->minval);
  pmv2(0.0, 0.0);
  pdr2(a->w, 0.0);
  setshade((Colorindex)a->maxval);
  pdr2(a->w, a->h/2.0);
  pdr2(0.0,  a->h/2.0);
  spclos();

  setshade((Colorindex)a->minval);
  pmv2(0.0, a->h/2.0);
  pdr2(a->w, a->h/2.0);
  setshade((Colorindex)a->maxval);
  pdr2(a->w, a->h);
  pdr2(0.0,  a->h);
  spclos();
#endif IRIS_4D
  
  popmatrix();
  if (a->beveled) drawbevel(a, p);
  if (a->label) drawlabel(a, p);
}


extern void _newvaltogglebutton();

defpanel()
{
  Alist *e;
  Actuator *a, *sa;
  Modslider *au;
  struct resetbutton *rb;
  Coord y, dy=0.5, yin;
  Coord x, dx;
  int i;
  
  initscriptpanel();
  
  panel=mkpanel();
  panel->label="colors";
  panel->ppu=40.0;
  panel->activefunc=constraincolorpanel;
  
  x=1;
  dx=1;
  y=0;
  
#define MK_RESET_BUTTON						\
  rb=PNL_MKUSERACT(resetbutton, pnl_wide_button);		\
  rb->button.label="R";						\
  rb->button.x=a->x;						\
  rb->button.y=a->y+a->h+PNL_DIM_1;				\
  rb->button.w=a->w;						\
  rb->targetact=a;						\
  rb->targetvar=NULL;						\
  rb->varinitval=a->val;					\
  rb->button.downfunc=resetact
		    
  x=0;
  
  { Typein *ad;
    
    a=lowcolortypein=mkact(pnl_typein);
    ad=(Typein *)a->data;
    sprintf(ad->str, "%d", basecolor);
    ad->len=5;
    a->x=x;
    a->y=y;
    a->upfunc=setcolorlimits;
    addact(a, panel);
    x+=a->w+PNL_DIM_1;
    
    a=highcolortypein=mkact(pnl_typein);
    ad=(Typein *)a->data;
    sprintf(ad->str, "%d", basecolor+ncolors);
    ad->len=5;
    a->x=x;
    a->y=y;
    a->upfunc=setcolorlimits;
    addact(a, panel);
    x+=a->w+PNL_DIM_1;
  }
  
  a=stopbutton=mkact(pnl_wide_button);
  a->newvalfunc=_newvaltogglebutton;
  a->label="stop";
  a->x=x;
  a->y=y;
  addact(a, panel);  
  x+=a->w+PNL_DIM_1;
  
  x=0;

  for (i=0;i<HUECHANNELS;i++) {
    y=2;
    
    a=hslider[i]=mkepslider(NULL, x, y, 0.0, 2.0, 1.0, NULL, WRAP, panel);

    a=lslider[i]=mkepslider(NULL, x, y, 0.0, 1.0, 0.5, NULL, WRAP, panel);
    a->visible=FALSE;
    ((Modslider *)(lslider[i]->u))->freqslider->visible=FALSE;
    ((Modslider *)(lslider[i]->u))->modebutton->visible=FALSE;
    fixact(lslider[i]);
    
    a=sslider[i]=mkepslider(NULL, x, y, 0.0, 1.0, 1.0, NULL, WRAP, panel);
    a->visible=FALSE;
    ((Modslider *)(sslider[i]->u))->freqslider->visible=FALSE;
    ((Modslider *)(sslider[i]->u))->modebutton->visible=FALSE;
    fixact(sslider[i]);
    
    if (i==HUECHANNELS-1) {
      huepalette=a=mkact(pnl_frame);
      PNL_ACCESS(Frame, a, mode)|=PNL_FM_FIXED;	/* don't resize */
      a->w=0.5-PNL_DIM_1;
      a->h=hslider[0]->h;
      a->x=x+hslider[0]->w+PNL_DIM_1;
      a->y=y;
      a->minval=SPECTRUM;
      a->maxval=SPECTRUM+SPECTRUMSIZE-1;
      a->drawfunc=drawdoublepalette;
      a->downfunc=togglechanmode;
      a->u="h";
      addact(a, panel);
      
      lightpalette=a=mkact(pnl_palette);
      a->w=0.5-PNL_DIM_1;
      a->h=hslider[0]->h;
      a->x=x+hslider[0]->w+PNL_DIM_1;
      a->y=y;
      a->minval=BWRAMP;
      a->maxval=BWRAMP+BWRAMPSIZE-1;
      a->visible=FALSE;
      a->downfunc=togglechanmode;
      a->u="l";
      addact(a, panel);

      satpalette=a=mkact(pnl_palette);
      a->w=0.5-PNL_DIM_1;
      a->h=hslider[0]->h;
      a->x=x+hslider[0]->w+PNL_DIM_1;
      a->y=y;
      a->minval=SATRAMP;
      a->maxval=SATRAMP+SATRAMPSIZE-1;
      a->visible=FALSE;
      a->downfunc=togglechanmode;
      a->u="s";
      addact(a, panel);
    }
    y+=6+4*PNL_ACCESS(Multislider, hslider[0], bh)+PNL_DIM_2
      +PNL_WIDE_BUTTON_HEIGHT+PNL_DIM_1;		/* hack */
    
    a=hueindex[i]=mkact(pnl_palette);
    a->h=PNL_BUTTON_EDGE;
    a->w=PNL_SLIDER_WIDTH;
    a->x=x;
    a->y=y;y+=a->h+PNL_DIM_1;
    a->minval=a->maxval=HUECHANINDEX+i;
    addact(a, panel);
    
    a=chanpositionrateslider[i]=mkact(pnl_hslider);
    a->w=PNL_SLIDER_WIDTH;
    a->h=2*PNL_ACCESS(Multislider, hslider[i], bh);
    a->x=x;
    a->y=y;
    a->visible=FALSE;
    PNL_ACCESS(Slider, a, mode)|=PNL_SM_NOSNAP;
    addact(a, panel);

    yin=y+a->h+PNL_DIM_1;;

    a=dwellslider[i]=mkact(pnl_filled_slider);
    a->h=3.0-PNL_DIM_1;
    a->x=x;
    a->y=y;y+=a->h+PNL_DIM_1;
    if (i==0) a->val=1;
    addact(a, panel);
    
    a=activebutton[i]=mkact(pnl_button);
    a->w=PNL_SLIDER_WIDTH;
    a->x=x;
    a->y=y;y+=a->h+PNL_DIM_1;
    if (i==0) a->val=1;
    addact(a, panel);
    
    x+=1;
  }
  
  ((Modslider *)(hslider[0]->u))->freqslider->val=0.01;
  
  a=chanrateslider=mkact(pnl_hslider);
  a->w=x-PNL_DIM_1;
  a->h=2*PNL_ACCESS(Multislider, hslider[0], bh);
  a->x=0;
  a->y=y;y+=a->h+PNL_DIM_1;
  addact(a, panel);
  
  a=mappalette=mkact(pnl_hpalette);
  a->w=x-PNL_DIM_1;
  a->x=0;
  a->y=y;y+=a->h;
  a->minval=basecolor;
  a->maxval=basecolor+ncolors;
  a->downfunc=toggleanimationmode;
  addact(a, panel);

  a=interpmodebutton=mkact(pnl_toggle_button);
  a->x=lightpalette->x;
  a->y=mappalette->y;
  a->w=lightpalette->w;
  a->h=mappalette->h;
  a->val=INTERP_RGB;
  a->minval=INTERP_RGB;
  a->maxval=INTERP_HLS;
  a->visible=FALSE;
  addact(a, panel);

  a=speedslider=mkact(pnl_slider);
  a->w=lightpalette->w;
  a->x=lightpalette->x;
  a->y=lightpalette->y+lightpalette->h+PNL_DIM_2;
  a->h=chanpositionrateslider[0]->y+chanpositionrateslider[0]->h-a->y;
  a->val=1;
  addact(a, panel);

  a=chanpositionmslider=mkact(pnl_hmultislider);
  a->w=x-PNL_DIM_1;
  a->x=0;
  a->y=yin;
  a->visible=FALSE;
  PNL_ACCESS(Multislider, a, n)=0;
  PNL_ACCESS(Multislider, a, mode)|=PNL_MSM_ORDERED;
  addact(a, panel);

  for (i=0;i<HUECHANNELS;i++) {
    a=chanpositionpalette[i]=mkact(pnl_palette);
    a->w=PNL_ACCESS(Multislider, chanpositionmslider, bh);
    a->h=chanpositionmslider->h;
    a->extval=i/(float)HUECHANNELS;
    a->minval=a->maxval=HUECHANINDEX+i;
    addsubact(a, chanpositionmslider);
  }
  /* don't do this at first to ensure that bars are in order */
  for (i=0;i<HUECHANNELS;i++) { 
    if (i!=0) chanpositionpalette[i]->extval=chanpositionmslider->maxval;
  }
  fixact(chanpositionmslider);
}

