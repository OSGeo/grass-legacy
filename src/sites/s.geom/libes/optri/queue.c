
#include "internoptri.h"
#include "queue.h"
#include "bitvector.h"

/*---------------------------------------------------------------------------*/

void
quReset (qu)

     queuesType *qu;

{
  qu->free = 0;
  qu->top = 0;
  qu->nofElts  = 0;
}

/*---------------------------------------------------------------------------*/

queuesType *
quNew (n, array, mode)

     int n;
     char * array;
     int mode;

{
  queuesType *qu;
  int i;

  if ((mode != POOL_FIXED_SIZE) && (mode != POOL_ADJUSTABLE_SIZE)) {
    printf("ERROR: quNew: mode inconsistent.\n");
    exit (1);
  }

  qu = MALLOC (queuesType, 1);
  qu->entry = MALLOC (indexType, n + 1);

  qu->in = 
    bvNew (n + 1, 
	   (mode == POOL_FIXED_SIZE ? BV_FIXED_SIZE : BV_ADJUSTABLE_SIZE));

  qu->maxn = n;
  qu->maxIndex = n;
  quReset (qu);
  qu->GT = 0;
  qu->data = array;
  qu->mode = mode;

  return (qu);
}

/*---------------------------------------------------------------------------*/

void
quAdjustSize (qu, newN)

     queuesType *qu;
     indexType newN;

{
  bvAdjustSize (qu->in, newN + 1);
  qu->maxIndex = 2 * newN;
}
  
/*---------------------------------------------------------------------------*/

queuesType *
pqStyleQuNew (n, array, dummy, mode)

     int n;
     char *array;
     int (*dummy) ();  /* in a priority queue the comparison function
			   takes the place of the dummy */
     int mode;

{
  return quNew (n, array, mode);
}

/*---------------------------------------------------------------------------*/

void
quDispose (qu)

     queuesType *qu;

{
  FREE (qu->entry);
  bvDispose (qu->in);
  FREE (qu);
}

/*---------------------------------------------------------------------------*/

void
quInsertNode (qu, newEntry)

     queuesType  *qu;
     indexType    newEntry;

{
  if (newEntry > qu->maxIndex)
    if (qu->mode == POOL_ADJUSTABLE_SIZE)
      quAdjustSize (qu, newEntry);
    else {
      printf ("quInsert: ERROR, Entry %d out of Bounds!\n", newEntry);
      exit (1);
    }

  if (qu->nofElts >= qu->maxn) {
    printf ("quInsert: SORRY, I'M FULL.\n");
    exit (1);
  }

  qu->entry[qu->free] = newEntry;
  bvSet (qu->in, newEntry);
  qu->free = (qu->free + 1) % qu->maxn;
  qu->nofElts++;
}

/*---------------------------------------------------------------------------*/

void
quDeleteNode (qu, oldEntry)

     queuesType *qu;
     indexType  oldEntry;
    
{
  indexType index;

  if (oldEntry > qu->maxIndex) {
    printf ("quDelete: ERROR, Entry %d out of Bounds!\n", oldEntry);
    exit (1);
  } else {
    if (bvTest (qu->in, oldEntry) == 0)
      printf 
	("quDelete: ERROR, deleting entry (%d) which is not in queue!\n",
	 oldEntry);
    else
      bvClear (qu->in, oldEntry);
  }
}

/*---------------------------------------------------------------------------*/

int
quContainsNode (qu, oldEntry)

     queuesType *qu;
     indexType  oldEntry;
    
{
  if (oldEntry > qu->maxIndex) {
    if (qu->mode == POOL_ADJUSTABLE_SIZE)
      return 0;
    else {
      printf ("quContainsNode: ERROR, Entry %d out of Bounds!\n", oldEntry);
      exit (1);
    }
  } else 
    return bvTest(qu->in, oldEntry) != 0;
}

/*---------------------------------------------------------------------------*/

int
quGetTop (qu, topEntry)

     queuesType  *qu;
     indexType *topEntry;
  
{
  while ((qu->nofElts > 0) && (! bvTest (qu->in, qu->entry[qu->top]))) {
    qu->top = (qu->top + 1) % qu->maxn;
    qu->nofElts--;
  }
  if (qu->nofElts == 0) 
    return 0;
  else {
    *topEntry = qu->entry[qu->top];
    return 1;
  }
}

/*---------------------------------------------------------------------------*/

void
quPrint (qu)

     queuesType *qu;

{
  int pointer, count;


  pointer = qu->top;
  count   = 0;
  while (count < qu->nofElts) {
    printf ("%d, ", pointer);
    printf ("%d, ", qu->entry[pointer]);
    pointer = (pointer + 1) % qu->maxn;
    count++;
    printf ("\n");
  }
  printf ("\n");
}

/*---------------------------------------------------------------------------*/

poolType * 
queue ()

{
  poolType *tmp;

  tmp = MALLOC (poolType, 1);

  tmp->create = pqStyleQuNew;
  tmp->top = quGetTop;
  tmp->insert = quInsertNode;
  tmp->delete = quDeleteNode;
  tmp->doesContain = quContainsNode;
  tmp->dispose = quDispose;

  return tmp;
}

/*---------------------------------------------------------------------------*/
/*
void
quTest ()

{
  queuesType *qu;
  int op, tmp;
  indexType a;

  qu = (*queue.create) (5, (char *) &op, (int *) 0);

  while (1) {
    scanf ("%d %d", &op, &tmp);
    a = tmp;
    printf ("%d %d %d", op, tmp, (int) a);
    if (op == 1) quInsertNode (qu, a);
    else if (op == 2) quDeleteNode (qu, a);
    else if (op == 3) quDeleteIndex (qu, a);
    else if (op == 4) {tmp = quGetTop (qu, &a); printf (" Top: %d %d", a, tmp);}
    else printf ("error ...\n");
    printf ("\n");quPrint (qu);
  }
}

main ()
{
  quTest ();
}
*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
