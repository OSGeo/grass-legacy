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

void
pnl_listadd(p, list)
     Alist *p, **list;
{
  p->next=(*list);
  (*list)=p;
}

void
pnl_listdelete(p, list)
     Alist *p, **list;
{     
  Alist *e;

  if (p==(*list)) {
    (*list)=(*list)->next;
  } else for (e=(*list);e;e=e->next) {
    if (e->next==p) {
      e->next=p->next;
      break;
    }
  }
}

Boolean
pnl_listin(p, list)
     Alist *p, *list;
{
  Alist *e;

  for (e=list;e;e=e->next) if (e->a==p->a) break;
  return (Boolean) e;
}

