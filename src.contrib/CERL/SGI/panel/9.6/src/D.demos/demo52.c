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
#include <sys/param.h>
#include <gl.h>
#include <device.h>
#include <panel.h>
  
  Actuator *s1, *s2, *b1, *b2, *b3, *b4, *rb1, *rb2, *rb3;
  short col=RED;

Panel
  *defpanel1(), *defpanel2(), *defpanel3();

/*
 *	demo52
 *
 *	just a standard demo, except configured to run nicely with 
 *	other processes.  Compare running 10 copies of this with
 *	10 copies of demo12.  Uses blocking pnl_dopanel().
 */

main() 
{
  Actuator *a;
  
  foreground();
  winopen("demo");
  doublebuffer();
  gconfig();
  
  defpanel1();
  defpanel2();
  defpanel3();
  
  pnl_needredraw();
  pnl_block=TRUE;		/* allow process to block when inactive */
  
  ortho2(-1.0,1.0,-1.0,1.0);

  drawit();
  swapbuffers();
  
  for (;;) { 			/* don't do graphics when inactive */
    while (!userredraw()&&!(a=pnl_dopanel()));

    if (a==b1) exit(0);
    if (a==b4) pnl_dumppanel();
    if (a==b2) {
      b3->val=b2->val;
      b3->dirtycnt=2;
    }
    if (a==rb1) col=CYAN;
    if (a==rb2) col=GREEN;
    if (a==rb3) col=RED;
    
    drawit();
    swapbuffers();
  }
}


drawit()
{
  pushmatrix();
  translate(s1->val,s2->val,0.0);
  color(BLACK);
  clear();
  color(col);
  rectf(-.10,-.10,.10,.10);
  popmatrix();
}


Panel
  *defpanel1()
{
  Actuator *a;
  Panel *panel;
  
  panel=pnl_mkpanel();
  panel->label="panel one";
  panel->ppu=50.0;
  
  s1=pnl_mkact(pnl_hslider);
  s1->label="y position";
  s1->x=1.0;
  s1->y=0.0;
  s1->minval= -1.0;
  s1->maxval=1.0;
  pnl_addact(s1, panel);
  
  b2=pnl_mkact(pnl_toggle_button);
  b2->label="push me";
  b2->x=1.0;
  b2->y=1.5;
  pnl_addact(b2, panel);
  
  b3=pnl_mkact(pnl_toggle_button);
  b3->label="I push myself";
  b3->x=1.0;
  b3->y=2.0;
  pnl_addact(b3, panel);
  
  a=pnl_mkact(pnl_typein);
  a->x=4.0;
  a->y=2.0;
  a->labeltype=PNL_LABEL_TOP;
  a->label="a typein window";
  PNL_ACCESS(Typein,a,str)="Edit This String";
  pnl_addact(a,panel);
  
  return panel;
}

Panel
  *defpanel2()
{
  Panel *panel;
  
  panel=pnl_mkpanel();
  panel->label="panel two";
  panel->ppu=50.0;
  
  s2=pnl_mkact(pnl_vslider);
  s2->label="x position";
  s2->x=0.0;
  s2->y=0.0;
  s2->minval= -1.0;
  s2->maxval=1.0;
  pnl_addact(s2, panel);
  
  b4=pnl_mkact(pnl_button);
  b4->label="dumppanel";
  b4->x=1.0;
  b4->y=1.5;
  pnl_addact(b4, panel);
  
  b1=pnl_mkact(pnl_button);
  b1->label="exit";
  b1->x=1.0;
  b1->y=1.0;
  pnl_addact(b1, panel);
  
  return panel;
}

Panel
  *defpanel3()
{
  Panel *panel;
  
  panel=pnl_mkpanel();
  panel->label="panel three";
  panel->ppu=50.0;
  
  rb3=pnl_mkact(pnl_radio_button);
  rb3->label="red";
  rb3->x=1.0;
  rb3->y=2.0;
  rb3->val=1.0;
  pnl_addact(rb3, panel);
  
  rb2=pnl_mkact(pnl_radio_button);
  rb2->label="green";
  rb2->x=1.0;
  rb2->y=1.5;
  pnl_addact(rb2, panel);
  
  rb1=pnl_mkact(pnl_radio_button);
  rb1->label="cyan";
  rb1->x=1.0;
  rb1->y=1.0;
  pnl_addact(rb1, panel);
  
  return panel;
}

