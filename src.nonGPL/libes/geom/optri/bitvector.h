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

/* bitvector.c */
void bvAdjustSize(bvType *, int);
bvType *bvNew(int, int);
void bvDispose(bvType *);
void bvReset(bvType *);
void bvSet(bvType *, int);
void bvClear(bvType *, int);
int bvTest(bvType *, int);
