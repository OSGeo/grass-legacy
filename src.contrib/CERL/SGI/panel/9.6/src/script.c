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

Actuator *filetypein;

void
beginscript()
{
  (void) pnl_beginwritescript(PNL_ACCESS(Typein, filetypein, str));
}

void
appendscript()
{
  (void) pnl_beginappendscript(PNL_ACCESS(Typein, filetypein, str));
}

void
endscript()
{
  (void) pnl_endwritescript();
}

void
readscript()
{
  (void) pnl_beginreadscript(PNL_ACCESS(Typein, filetypein, str));
}

void
continuescript()
{
  (void) pnl_continuereadscript(PNL_ACCESS(Typein, filetypein, str));
}

void
stopscript()
{
  (void) pnl_endreadscript();
}

void
setreadbutton(a)
Actuator *a;
{
  if (pnl_readscript) {
    if (a->val!=a->maxval) {
      a->val=a->maxval;
      pnl_fixact(a);
    }
  } else {
    if (a->val!=a->minval) {
      a->val=a->minval;
      pnl_fixact(a);
    }
  }
}

void
setwritebutton(a)
Actuator *a;
{
  if (pnl_writescript) {
    if (a->val!=a->maxval) {
      a->val=a->maxval;
      pnl_fixact(a);
    }
  } else {
    if (a->val!=a->minval) {
      a->val=a->minval;
      pnl_fixact(a);
    }
  }
}

void
setdontdraw(a)
Actuator *a;
{
  pnl_dont_draw=a->val==a->maxval;
}

void
setignoredelay(a)
Actuator *a;
{
  pnl_ignore_delay=a->val==a->maxval;
  if (!pnl_ignore_delay) pnl_frame_number=pnl_delay;  
    /* don't start delaying until next delay */
}

extern void _newvaltogglebutton();

initscriptpanel()
{
  Actuator *a;
  Panel *p;
  float x=0.0, y=0.0, dy=0.5;
  
  p=pnl_mkpanel();
  p->label="scripting";
  
  a=filetypein=pnl_mkact(pnl_typein);
  a->label="script file";
  a->x=x;
  a->y=(y-=dy);
  a->labeltype=PNL_LABEL_BOTTOM_RIGHT;
  PNL_ACCESS(Typein, a, str)="panel.script";
  pnl_addact(a, p);
  
#ifdef 0
  a=pnl_mkact(pnl_button);
  a->label="dump state";
  a->x=x;
  a->y=(y-=dy);
  a->downfunc=pnl_dumpstate;
  pnl_addact(a, p);
#endif
  
  a=pnl_mkact(pnl_button);
  a->label="begin write";
  a->x=x;
  a->y=(y-=dy);
  a->downfunc=beginscript;
  pnl_addact(a, p);
  
  a=pnl_mkact(pnl_button);
  a->label="begin append";
  a->x=x;
  a->y=(y-=dy);
  a->downfunc=appendscript;
  pnl_addact(a, p);
  
  a=pnl_mkact(pnl_button);
  a->label="stop write/append";
  a->x=x;
  a->y=(y-=dy);
  a->downfunc=endscript;
  pnl_addact(a, p);
  
  y-=dy;
  
  a=pnl_mkact(pnl_button);
  a->label="begin read";
  a->x=x;
  a->y=(y-=dy);
  a->downfunc=readscript;
  pnl_addact(a, p);
  
  a=pnl_mkact(pnl_button);
  a->label="continue read";
  a->x=x;
  a->y=(y-=dy);
  a->downfunc=continuescript;
  pnl_addact(a, p);
  
  a=pnl_mkact(pnl_button);
  a->label="stop read";
  a->x=x;
  a->y=(y-=dy);
  a->downfunc=stopscript;
  pnl_addact(a, p);
  
  y+=6*dy;
  x+=4;

  a=pnl_mkact(pnl_wide_button);
  a->label="READING";
  a->x=x;
  a->y=(y-=dy);
  a->val=a->minval;
  a->newvalfunc=NULL;
  a->activefunc=setreadbutton;
  a->automatic=TRUE;
  pnl_addact(a, p);
  
  a=pnl_mkact(pnl_wide_button);
  a->label="WRITING";
  a->x=x;
  a->y=(y-=dy);
  a->val=a->minval;
  a->newvalfunc=NULL;
  a->activefunc=setwritebutton;
  a->automatic=TRUE;
  pnl_addact(a, p);

  y-=2*dy;

  a=pnl_mkact(pnl_wide_button);
  a->label="DONT DRAW";
  a->w=3;
  a->x=x;
  a->y=(y-=dy);
  a->val=a->minval;
  a->newvalfunc=_newvaltogglebutton;
  a->upfunc=setdontdraw;
  pnl_addact(a, p);

  a=pnl_mkact(pnl_wide_button);
  a->label="IGNORE DELAYS";
  a->w=3;
  a->x=x;
  a->y=(y-=dy);
  a->val=a->minval;
  a->newvalfunc=_newvaltogglebutton;
  a->upfunc=setignoredelay;
  pnl_addact(a, p);

}
