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

#define LOWCOLOR    0
#define HIGHCOLOR   1023
#define BASHR	    0
#define BASHG	    0
#define BASHB	    0
#define MAXRANGE    1024

Panel
*defpanel();

Colorindex range, bash=LOWCOLOR, lastbash=LOWCOLOR;
float rangef, bashf=(float)LOWCOLOR;
short *savr, *savg, *savb, bashr,  bashg,  bashb;

Actuator *index_slider, *range_slider;

main() 
{
    savr=(short *)malloc(sizeof(Colorindex)*MAXRANGE);
    savg=(short *)malloc(sizeof(Colorindex)*MAXRANGE);
    savb=(short *)malloc(sizeof(Colorindex)*MAXRANGE);

    foreground();
    noport();
    winopen("mapo");

    doublebuffer();
    gconfig();

    defpanel();

    for (;;) {
        pnl_dopanel();
	swapbuffers();
    }
}

void
savecolors(start, end)
Colorindex start, end;
{
int temp, i, dir;

    if (start>end) {
	temp=end;
	end=start;
	start=temp;
    }
    for (i=start;i<=end;i++) {
	getmcolor(i, &savr[i], &savg[i], &savb[i]);
    }
}

void
bashcolors(start, end)
Colorindex start, end;
{
int temp, i, dir;

    if (start>end) {
	temp=end;
	end=start;
	start=temp;
    }
    for (i=start;i<=end;i++) {
	mapcolor(i, bashr, bashg, bashb);
    }
}

void
restorecolors(start, end)
Colorindex start, end;
{
int temp, i, dir;

    if (start>end) {
	temp=end;
	end=start;
	start=temp;
    }
    for (i=start;i<=end;i++) {
	mapcolor(i, savr[i], savg[i], savb[i]);
    }
}

void
bashmap(a)
Actuator *a;
{
short tr, tg, tb;

    bashf+=index_slider->val;
    rangef=range_slider->val;
    bashf=MAX((float)LOWCOLOR+rangef, bashf);
    bashf=MIN((float)HIGHCOLOR-rangef, bashf);

    bash=(Colorindex)bashf;
    range=(Colorindex)rangef;
    if (lastbash!=bash) {
	savecolors(bash, bash+range);
	bashcolors(bash, bash+range);
	restorecolors(lastbash, lastbash+range);
        lastbash=bash;
    }
}

void
patchmap(){
}

void
cleanexit()
{
    exit(0);
}

Panel
*defpanel()
{
Panel *p;
Actuator *a;

    p=pnl_mkpanel();
    p->label="mapo";

    a=pnl_mkact(pnl_button);
    a->label="exit";
    a->x=1.0;
    a->y=1.0;
    a->downfunc=cleanexit;
    pnl_addact(a, p);

    a=pnl_mkact(pnl_slider);
    a->label="index";
    a->x=1.0;
    a->y=2.5;
    a->val=0.0;
    a->maxval=  (float)(HIGHCOLOR-LOWCOLOR)/100.0;
    a->minval= -(float)(HIGHCOLOR-LOWCOLOR)/100.0;
    a->activefunc=bashmap;
    index_slider=a;
    pnl_addact(a, p);

    a=pnl_mkact(pnl_slider);
    a->label="range";
    a->x=2.0;
    a->y=2.5;
    a->val=0.0;
    a->maxval=  (float)(HIGHCOLOR-LOWCOLOR)/10.0;
    a->minval= -(float)(HIGHCOLOR-LOWCOLOR)/10.0;
    a->activefunc=bashmap;
    range_slider=a;
    pnl_addact(a, p);
}

