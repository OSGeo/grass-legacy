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
*defpanel();

main()
{
  Actuator *a;
  Panel *panel;

  foreground();
  noport();
  winopen("demo");

  doublebuffer();
  gconfig();

  ortho2(-1.0,1.0,-1.0,1.0);

  panel=defpanel();

  for (;;) {
    pnl_dopanel();
    swapbuffers();
  }
}

/* note, default position moves down after creating bigbuttons... */

#define MKBIGBUTTON()	\
  a=pnl_mkact(pnl_wide_button);	\
  a->w=1.0-PNL_DIM_1;		\
  a->h=1.0-PNL_DIM_1;		\
  a->x=x;			\
  a->y=(y-=d);			\
  pnl_addact(a, p)

/* ...and to the right after creating these dials */

#define MKDIAL()       	   	\
  a=pnl_mkact(pnl_dial);	   	\
  a->w=a->h*=1.5;	   	\
  a->x=x;		   	\
  a->y=y;		   	\
  x+=a->w+(1.6667*PNL_DIM_1);	\
  pnl_addact(a, p)

Panel
*defpanel()
{
  Actuator *a;
  Panel *p;
  Coord y, x, d=1.0;

  p=pnl_mkpanel();
  p->label="demo";
  p->ppu=50.0;

  x=0.0;
  y=0.0;

  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();

  x=1.0;
  y=1.0;

  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();

  x=2.0;
  y=1.0;

  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();

  x=3.0;
  y=1.0;

  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();

  x=4.0;
  y=1.0;

  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();

  x=5.0;
  y=0.0;

  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();
  MKBIGBUTTON();

  x= 0.0;
  y= -6.5;

  MKDIAL();
  MKDIAL();
  MKDIAL();
  MKDIAL();

  x= 0.0;
  y= -8.0;

  MKDIAL();
  MKDIAL();
  MKDIAL();
  MKDIAL();

  return p;
}

