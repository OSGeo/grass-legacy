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

Panel
*defpanel1(), *defpanel2();

void (*drawfunc)();
void (*savedfunc)();
long snorkseed;
short col;

main() 
{
    foreground();
    winopen("demo");
    doublebuffer();
    gconfig();

    ortho2(-1.0,1.0,-1.0,1.0);

    defpanel1();
    defpanel2();

    pnl_needredraw();

#ifdef IRIS_4D
    (*drawfunc)();
    for (;;) {
        if (pnl_dopanel()) {
	  do {
	    (*drawfunc)();
	  } while (pnl_dopanel());
	  (*drawfunc)();
	}
	if (pnl_userredraw()) {
	  reshapeviewport();
	  ortho2(-1.0,1.0,-1.0,1.0);
	  (*drawfunc)();
	}
    }
#else  IRIS_4D
    (*drawfunc)();
    (*drawfunc)();
    for (;;) {
        if (pnl_dopanel()) {
	    do {
		(*drawfunc)();
	    } while (pnl_dopanel());
	    (*drawfunc)();
	    pnl_drawpanel();
	    (*drawfunc)();
	    (*drawfunc)(); /* must have even number here */
	}
	if (pnl_userredraw()) {
	  reshapeviewport();
	  ortho2(-1.0,1.0,-1.0,1.0);
	  (*drawfunc)();
	  (*drawfunc)();  /* must have even number here */
	}
	swapbuffers();
    }
#endif IRIS_4D
}

void
setcol(a)
Actuator *a;
{
    if (!strcmp(a->label,"green"))  col=GREEN;
    if (!strcmp(a->label,"cyan"))   col=CYAN;
    if (!strcmp(a->label,"red"))    col=RED;
}

void
cleanexit()
{
    exit(0);
}

void
drawstar()
{
    color(BLACK);
    clear();
    color(col);
    move2(-0.6,-0.7);
    draw2( 0.0, 0.7);
    draw2( 0.6,-0.7);
    draw2(-0.7, 0.2);
    draw2( 0.7, 0.2);
    draw2(-0.6,-0.7);

    swapbuffers();
}

void
drawcross()
{
    color(BLACK);
    clear();
    color(col);
    rect(-0.2,-0.7, 0.2, 0.7);
    rect(-0.7,-0.2, 0.7, 0.2);

    swapbuffers();
}

void
drawsnork()
{
int i;
float x,y;    

    srand(snorkseed);

    color(BLACK);
    clear();
    color(col);
    move2(0.0,0.0);
    for (i=0;i<100;i++) {
	x=(float)(rand()%500-250)/275.0;
	y=(float)(rand()%500-250)/275.0;
	draw2(x,y);
    }

    swapbuffers();
}

void setstar()	  { drawfunc=drawstar; }
void setcross()	  { drawfunc=drawcross; }
void setsnork()	  { drawfunc=drawsnork; }
void savefunc()   { savedfunc=drawfunc; }
void newsnork()	  { 
    setsnork();
    snorkseed+=3;
}
void restorefunc(a) { drawfunc=savedfunc; }
void setgreen()	  { col=GREEN; }
void setred()	  { col=RED; }
void setcyan()    { col=CYAN; }

Panel
*defpanel1()
{
Panel *p;
Actuator *a;

    initscriptpanel();

    p=pnl_mkpanel();
    p->label="colors";

    a=pnl_mkact(pnl_label);
    a->label="color controls";
    a->x=1.0;
    a->y=4.0;
    pnl_addact(a, p);

    a=pnl_mkact(pnl_wide_button);
    a->label="exit";
    a->x=1.0;
    a->y=1.0;
    a->downfunc=cleanexit;
    pnl_addact(a, p);

    a=pnl_mkact(pnl_radio_button);
    a->label="red";
    a->x=1.0;
    a->y=3.0;
    a->downfunc=setcol;
    a->val=1.0;
    setred();
    pnl_addact(a, p);

    a=pnl_mkact(pnl_radio_button);
    a->label="green";
    a->x=1.0;
    a->y=2.5;
    a->downfunc=setgreen;
    pnl_addact(a, p);

    a=pnl_mkact(pnl_radio_button);
    a->label="cyan";
    a->x=1.0;
    a->y=2.0;
    a->downfunc=setcyan;
    pnl_addact(a, p);

    pnl_endgroup(p);
}

Panel
*defpanel2()
{
Panel *p;
Actuator *a;

    p=pnl_mkpanel();
    p->label="shapes";

    a=pnl_mkact(pnl_label);
    a->label="shape controls";
    a->x=1.0;
    a->y=4.0;
    pnl_addact(a, p);

    a=pnl_mkact(pnl_radio_button);
    a->label="star";
    a->x=1.0;
    a->y=3.0;
    a->downfunc=setstar;
    a->val=1.0;
    setstar();
    pnl_addact(a, p);

    a=pnl_mkact(pnl_radio_button);
    a->label="cross";
    a->x=1.0;
    a->y=2.5;
    a->downfunc=setcross;
    pnl_addact(a, p);

    a=pnl_mkact(pnl_radio_button);
    a->label="snork";
    a->x=1.0;
    a->y=2.0;
    a->downfunc=setsnork;
    pnl_addact(a, p);

    pnl_endgroup(p);

    a=pnl_mkact(pnl_wide_button);
    a->label="make new snorks";
    a->w=4.0;
    a->x=1.0;
    a->y=1.0;
    a->downfunc=savefunc;
    a->activefunc=newsnork;
    a->upfunc=restorefunc;
    pnl_addact(a, p);
}

