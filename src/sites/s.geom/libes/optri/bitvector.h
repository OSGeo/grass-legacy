/*
 * NAME: bitvector.h
 *
 * DESCRIPTION:
 *	This file contains type definitions and function prototypes
 *	supporting a bitvector data structure.
 *
 * HISTORY:
 *	89/10/26: This header added
 */


typedef unsigned int bvWordType;

typedef struct {
  bvWordType *bvwords;
  bvWordType lgbits;	/* log2(bits per word) */
  int maxn;
  int mode;
} bvType;

#define BV_FIXED_SIZE -435643626
#define BV_ADJUSTABLE_SIZE 24524354

extern bvType	*bvNew(/* maxn */);
extern void	bvDispose(/* bv */);

extern void	bvReset(/* bv */);
extern void	bvSet(/* bv, i */);
extern void	bvClear(/* bv, i */);
extern int	bvTest(/* bv, i */);

