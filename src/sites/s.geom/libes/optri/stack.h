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

extern stackType	*stkNew(/* maxn */);
extern void		stkDispose(/* stk */);

extern void		stkReset(/* stk */);
extern void		stkPush(/* stk, i */);
extern void		stkPop(/* stk, ip */);
extern int		stkEmpty(/* stk */);
