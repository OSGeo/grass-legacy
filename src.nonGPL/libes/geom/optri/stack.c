
#include <stdio.h>
#include "geom/basic.h"
#include "stack.h"

stackType *
stkNew (int n)
{
  stackType *stk;

  stk = MALLOC (stackType, 1);
  stk->stkwords = MALLOC (stackEntry, n);

  stk->maxn = n;
  stk->sp = 0;

  return(stk);
}  /* -------------------- stkNew -------------------- */


void 
stkDispose (stackType *stk)
{
  FREE (stk->stkwords);
  FREE (stk);
}  /* -------------------- stkDispose -------------------- */


void 
stkReset (stackType *stk)
{
  stk->sp = 0;
}  /* -------------------- stkReset --------------------- */


void 
stkPush (stackType *stk, int i)

{
  void (*xx) ();

  xx = 0;
  
  if (stk->sp < stk->maxn)
    stk->stkwords[stk->sp++] = i;

  else {
    (void) fprintf(stdout, "stkPush: stack overflow.\n");
    exit (1);
    xx ();
  }
}  /* -------------------- stkPush -------------------- */


void 
stkPop (stackType *stk, int *ip)
{
  if (stk->sp)
    *ip = stk->stkwords[--stk->sp];

  else
    (void) fprintf(stdout, "stkPop: stack underflow.\n");
}  /* -------------------- stkPop -------------------- */


int 
stkEmpty (stackType *stk)
{
  return(! stk->sp);
}  /* -------------------- stkEmpty -------------------- */
