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
#include <setjmp.h>
#include <signal.h>
#include <device.h>
#include <gl.h>
#include <panel.h>

/* this file implements the signal actuator */

extern jmp_buf func_return;

void
pnl_sighandler(sig)
     int sig;
{
  signal(sig, SIG_IGN);

  if (pnl_sl[sig]->downfunc==SIG_DFL) exit(2);
  if (pnl_sl[sig]->downfunc==SIG_IGN) ;	/* nothin' */ 
  else
    if (pnl_sl[sig]->downfunc) {
      pnl_funcmode=PNL_FCNM_DOWN;
      (*pnl_sl[sig]->downfunc)(pnl_sl[sig]);
      pnl_funcmode=PNL_FCNM_NONE;
    }
  
  qenter(PNL_TOKEN_SIGNAL, sig);

  signal(sig, pnl_sighandler);

  if (pnl_funcmode!=PNL_FCNM_NONE) {
    longjmp(func_return);
  }
}

void
_newvalsignal(a, p, x, y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{
  Signal *ad=(Signal *)a->data;
/*
  a->active=ad->activate;
  ad->activate=FALSE;
*/
}

void
_addsignal(a, p)
Actuator *a;
Panel *p;
{
  Signal *ad=(Signal *)a->data;

  if (pnl_sl[ad->signal]) {
    printf("panellib: replacing signal actuator for signal %d\n", signal);
    delact(pnl_sl[ad->signal]);
  }
      
  pnl_sl[ad->signal]=a;
  signal(ad->signal, pnl_sighandler);
}

void
_delsignal(a, p)
Actuator *a;
Panel *p;
{
  Signal *ad=(Signal *)a->data;

  pnl_sl[ad->signal]=NULL;
  signal(ad->signal, SIG_DFL);
}

void
pnl_signal(a)
Actuator *a;
{
  Signal *ad;
  a->type=PNL_SIGNAL;

  ad=(Signal *)(a->data=(char *)pnl_alloc(sizeof(Signal)));
  a->datasize=sizeof(Signal);
  ad->signal=SIGINT;		/* default signal */
  a->newvalfunc=_newvalsignal;
  a->addfunc=_addsignal;
  a->delfunc=_delsignal;
  a->downfunc=SIG_IGN;
  a->visible=FALSE;
}
