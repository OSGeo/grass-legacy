
#include <stdio.h>
#include "geom/basic.h"
#include "bitvector.h"

/* this modul does not work when geom/basic.h is included. */
/* so here goes the hack ... cpied from geom/basic.h */

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

void 
bvAdjustSize (bvType *bv, int newN)

{
  int i;
  unsigned int bitsPerWord, nwordsBefore, nwordsAfter;

  REALLOC (bv->bvwords, bvWordType, 2 * newN + 1);

  bitsPerWord = sizeof(bvWordType) << 3;
  nwordsBefore = bv->maxn / bitsPerWord + 1;
  nwordsAfter = (2 * newN) / bitsPerWord + 1;

  for (i = nwordsBefore; i < nwordsAfter; i++)
    bv->bvwords[i] = 0;

  bv->maxn = 2 * newN;
}
/*---------------------------------------------------------------------------*/
/*   bvNew allocates a bitvector with bits to toggle for 0 through maxn.
 */
bvType *
bvNew (int maxn, int mode)
{
  bvType *bv;
  unsigned int bitsPerWord, nwords;

  bitsPerWord = sizeof(bvWordType) << 3;
  nwords = maxn / bitsPerWord + 1;

  bv = MALLOC (bvType, 1);
  bv->bvwords = MALLOC (bvWordType, nwords);

  bv->maxn = maxn;

  if ((mode != BV_FIXED_SIZE) && (mode != BV_ADJUSTABLE_SIZE)) {
    fprintf (stdout,"ERROR: bvNew: mode inconsistent.\n");
    exit (1);
  }
  bv->mode = mode;

  /* calculate log2(bitsPerWord) */
  bv->lgbits = 0;
  while (bitsPerWord >>= 1)
    bv->lgbits++;

  bvReset (bv);

  return(bv);
}  /* -------------------- bvNew -------------------- */
  

void 
bvDispose (bvType *bv)
{
  FREE (bv->bvwords);
  FREE (bv);
}  /* -------------------- bvDispose ------------------- */


void 
bvReset (bvType *bv)
{
  int i;
  unsigned int bitsPerWord, nwords;

  bitsPerWord = sizeof(bvWordType) << 3;
  nwords = bv->maxn / bitsPerWord + 1;

  for (i=0; i < nwords; i++)
    bv->bvwords[i] = 0;
}  /* -------------------- bvReset -------------------- */


void 
bvSet (bvType *bv, int i)
{
  bvWordType modMask = (sizeof(bvWordType)<<3) - 1;

  if (i > bv->maxn)
    if (bv->mode == BV_ADJUSTABLE_SIZE)
      bvAdjustSize (bv, i);
    else {
      (void) fprintf(stdout, "bvSet: bounds error.\n");
      exit (1);
    }
  if (i < 0) {
    (void) fprintf(stdout, "bvSet: bounds error.\n");
    exit (1);
  }

  bv->bvwords[i >> bv->lgbits] |= 1 << (i & modMask);
}  /* -------------------- bvSet -------------------- */


void 
bvClear (bvType *bv, int i)
{
  unsigned int word = i >> bv->lgbits;	
  bvWordType modMask = (sizeof(bvWordType)<<3) - 1;
  bvWordType setBit;

  setBit = 1 << (i & modMask);

  if (0 <= i && i <= bv->maxn) {
    bv->bvwords[word] |= setBit;	/* make sure bit is set */
    bv->bvwords[word] ^= setBit;	/* toggle it off with xor */
  }
  else {
    (void) fprintf(stdout, "bvClear: bounds error.\n");
    exit (1);
  }
}  /* -------------------- bvClear -------------------- */


int 
bvTest (bvType *bv, int i)
{
  bvWordType modMask = (sizeof(bvWordType)<<3) - 1;

  if (0 <= i && i <= bv->maxn)
    return(bv->bvwords[i >> bv->lgbits] & (1 << (i & modMask)));

  /* else ... */
  (void) fprintf(stdout, "bvTest: bounds error.\n");
  exit (1);
}  /* -------------------- bvTest -------------------- */
