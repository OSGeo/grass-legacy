/*
 * NAME: stack.h
 *
 * DESCRIPTION:
 *	This is simply a header file supporting stacks.
 *
 * HISTORY:
 *	89/10/26: This header added
 */


typedef int stackEntry;

typedef struct {
  stackEntry *stkwords;
  int sp, vp;
  int maxn;
} stackType;

/* stack.c */
stackType *stkNew(int);
void stkDispose(stackType *);
void stkReset(stackType *);
void stkPush(stackType *, int);
void stkPop(stackType *, int *);
int stkEmpty(stackType *);
