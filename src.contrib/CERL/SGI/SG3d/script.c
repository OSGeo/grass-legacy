
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/

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
**  These routines taken by dpg so I could create my own script panel
*/
#include <gl.h>
#include <device.h>
#include <panel.h>

Actuator *filetypein;

extern int Write_script;

void
my_beginscript()
{
  (void) pnl_beginwritescript(PNL_ACCESS(Typein, filetypein, str));
}

void
my_appendscript()
{
  (void) pnl_beginappendscript(PNL_ACCESS(Typein, filetypein, str));
}

void
my_endscript()
{
  (void) pnl_endwritescript();
}

void
my_readscript()
{
  (void) pnl_beginreadscript(PNL_ACCESS(Typein, filetypein, str));
}

void
my_continuescript()
{
  (void) pnl_continuereadscript(PNL_ACCESS(Typein, filetypein, str));
}

void
my_stopscript()
{
  (void) pnl_endreadscript();
}

void
my_setreadbutton(a)
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
my_setwritebutton(a)
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
my_setdontdraw(a)
Actuator *a;
{
  pnl_dont_draw=a->val==a->maxval;
}

void
my_setignoredelay(a)
Actuator *a;
{
  pnl_ignore_delay=a->val==a->maxval;
  if (!pnl_ignore_delay) pnl_frame_number=pnl_delay;  
    /* don't start delaying until next delay */
}

extern void _newvaltogglebutton();

Panel *
my_initscriptpanel()
{
    char *ptr, *getenv();
    Actuator *a;
    Panel *p;
    float x=0.0, y=0.0, dy=0.5;

    ptr = getenv ("WRITE");

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

    y-=3*dy; /* mine */
    a=pnl_mkact(pnl_button);
    a->label="begin write";
    a->x=x;
    a->y=(y-=dy);
    a->downfunc=my_beginscript;
    if (!ptr && !Write_script)	/* Write disabled */
	a->visible = 0;
    pnl_addact(a, p);

    a=pnl_mkact(pnl_button);
    a->label="begin append";
    a->x=x;
    a->y=(y-=dy);
    a->downfunc=my_appendscript;
    if (!ptr && !Write_script)	/* Write disabled */
	a->visible = 0;
    pnl_addact(a, p);

    a=pnl_mkact(pnl_button);
    a->label="stop write/append";
    a->x=x;
    a->y=(y-=dy);
    a->downfunc=my_endscript;
    if (!ptr && !Write_script)	/* Write disabled */
	a->visible = 0;
    pnl_addact(a, p);

    y-=dy;

    y = 0-2*dy;  /* mine */
    a=pnl_mkact(pnl_wide_button);
    a->label="begin read";
    a->x=x;
    a->y=(y-=dy);
    a->downfunc=my_readscript;
    pnl_addact(a, p);

    a=pnl_mkact(pnl_wide_button);
    a->label="stop read";
    a->x=x;
    a->y=(y-=dy);
    a->downfunc=my_stopscript;
    pnl_addact(a, p);

    /*
    a=pnl_mkact(pnl_wide_button);
    a->label="continue read";
    a->x=x;
    a->y=(y-=dy);
    a->downfunc=my_continuescript;
    pnl_addact(a, p);
    */


    /*y+=6*dy;*/
    /*y+=5*dy;*/
    y =0 - 2*dy;
    x+=4;

    a=pnl_mkact(pnl_wide_button);
    a->label="READING";
    a->x=x;
    a->y=(y-=dy);
    a->val=a->minval;
    a->newvalfunc=NULL;
    a->activefunc=my_setreadbutton;
    a->automatic=TRUE;
    pnl_addact(a, p);

    {
    a=pnl_mkact(pnl_wide_button);
    a->label="WRITING";
    a->x=x;
    a->y=(y-=dy);
    a->val=a->minval;
    a->newvalfunc=NULL;
    a->activefunc=my_setwritebutton;
    a->automatic=TRUE;
    if (!ptr && !Write_script)	/* Write disabled */
	a->visible = 0;
    pnl_addact(a, p);
    }

    /*y-=2*dy;*/
    y-=dy;

    /*
    a=pnl_mkact(pnl_wide_button);
    a->label="DONT DRAW";
    a->w=3;
    a->x=x;
    a->y=(y-=dy);
    a->val=a->minval;
    a->newvalfunc=_newvaltogglebutton;
    a->upfunc=my_setdontdraw;
    pnl_addact(a, p);

    a=pnl_mkact(pnl_wide_button);
    a->label="IGNORE DELAYS";
    a->w=3;
    a->x=x;
    a->y=(y-=dy);
    a->val=a->minval;
    a->newvalfunc=_newvaltogglebutton;
    a->upfunc=my_setignoredelay;
    pnl_addact(a, p);
    */

    return (p);

}
