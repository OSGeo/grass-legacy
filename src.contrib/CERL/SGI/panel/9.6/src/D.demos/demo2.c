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

Actuator *s1, *s2, *b1;

Panel
*defpanel();

main() 
{
Actuator *a;
Panel *panel;

    foreground();
    winopen("demo");
    doublebuffer();
    gconfig();

    ortho2(-1.0,1.0,-1.0,1.0);

    panel=defpanel();

    for (;;) {
        a=pnl_dopanel();
	if (a==b1) {
	    exit(0);
	}
	s2->w=s1->val*10.0;
	s2->dirtycnt=2;
	panel->dirtycnt=2;	/* because we are modifying the layout */
	pushmatrix();
	translate(s1->val,s2->val,0.0);
	color(BLACK);
	clear();
	color(WHITE);
	drawit();
	swapbuffers();
	popmatrix();
    }
}


drawit()
{
    rectf(-.10,-.10,.10,.10);
}


Panel
*defpanel()
{
Panel *panel;

    panel=pnl_mkpanel();

    s1=pnl_mkact(pnl_slider);
    s1->label="slider 1";
    s1->x=0.0;
    s1->y=0.0;
    s1->minval= -1.0;
    s1->maxval=1.0;
    pnl_addact(s1, panel);

    s2=pnl_mkact(pnl_slider);
    s2->label="slider 2";
    s2->x=1.0;
    s2->y=0.0;
    s2->minval= -1.0;
    s2->maxval=1.0;
    pnl_addact(s2, panel);

    b1=pnl_mkact(pnl_button);
    b1->label="button 1";
    b1->x=2.0;
    b1->y=0.0;
    pnl_addact(b1, panel);

    return panel;
}

