
#include <stdio.h>
#include "basic.h"
#include "stack.h"

stackType *stkNew(n)
int n;
{
  stackType *stk;

  stk = MALLOC (stackType, 1);
  stk->stkwords = MALLOC (stackEntry, n);

  stk->maxn = n;
  stk->sp = 0;

  return(stk);
}  /* -------------------- stkNew -------------------- */


void stkDispose(stk)
stackType *stk;
{
  FREE (stk->stkwords);
  FREE (stk);
}  /* -------------------- stkDispose -------------------- */


void stkReset(stk)
stackType *stk;
{
  stk->sp = 0;
}  /* -------------------- stkReset --------------------- */


void stkPush(stk, i)
stackType *stk;
int i;

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


void stkPop(stk, ip)
stackType *stk;
int *ip;
{
  if (stk->sp)
    *ip = stk->stkwords[--stk->sp];

  else
    (void) fprintf(stdout, "stkPop: stack underflow.\n");
}  /* -------------------- stkPop -------------------- */


int stkEmpty(stk)
stackType *stk;
{
  return(! stk->sp);
}  /* -------------------- stkEmpty -------------------- */
