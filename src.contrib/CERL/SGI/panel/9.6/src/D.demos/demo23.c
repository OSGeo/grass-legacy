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

Actuator *s1, *s2, *s3, *b1, *b2, *b3;

void defpanel();

main() 
{
Actuator *a;
short col=RED;

    foreground();
    winopen("demo");
    doublebuffer();
    gconfig();

    defpanel();

    for (;;) {
        a=pnl_dopanel();
	if (a==b1) s1->visible= !(b1->val==1.0);
	if (a==b2) s2->visible= !(b2->val==1.0);
	if (a==b3) s3->visible= !(b3->val==1.0);

	color(BLACK);
	clear();
	swapbuffers();
    }
}


void
defpanel()
{
Panel *p;

    p=pnl_mkpanel();
    p->label="fixpanel demo";
    p->ppu=50.0;
    p->upfunc=p->fixfunc;

    s1=pnl_mkact(pnl_slider);
    s1->label="slider 1";
    s1->x=4.0;
    s1->y=0.0;
    pnl_addact(s1, p);

    s2=pnl_mkact(pnl_hslider);
    s2->label="slider 2";
    s2->x=0.0;
    s2->y= -2.0;
    pnl_addact(s2, p);

    s3=pnl_mkact(pnl_slider);
    s3->label="slider 3";
    s3->x= -2.0;
    s3->y=0.0;
    pnl_addact(s3, p);

    b1=pnl_mkact(pnl_toggle_button);
    b1->label="slider 1";
    b1->x=1.0;
    b1->y=1.0;
    pnl_addact(b1, p);

    b2=pnl_mkact(pnl_toggle_button);
    b2->label="slider 2";
    b2->x=1.0;
    b2->y=1.5;
    pnl_addact(b2, p);

    b3=pnl_mkact(pnl_toggle_button);
    b3->label="slider 3";
    b3->x=1.0;
    b3->y=2.0;
    pnl_addact(b3, p);
}

