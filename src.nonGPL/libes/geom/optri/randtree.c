
#include <sys/time.h>
#include "geom/basic.h"
#include "stack.h"
#include "randtree.h"

/*--------------------------------------------------------------------------*/

#define NIL -1

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static int rsGetParent (rsType *T, int x)

{
  void (*tmp)() = NULL;
  if (x != T->lastX)
    if ((x == T->parentX) && (x != NIL)) {
      T->lastX = T->parentX;
      
      if (! stkEmpty (T->parentStack)) 
        stkPop (T->parentStack, &(T->parentX)) ;
      else
        T->parentX = NIL;
    } else {
      fprintf (stdout,"ERROR: rsParent: parent inconsistency\n");
      tmp ();
      exit (1);
    }

  return T->parentX;
}

/*--------------------------------------------------------------------------*/

#define rsResetParent(x) (T->lastX = (x), T->parentX = NIL, \
			  stkReset (T->parentStack))

#define rsPushParent(p) ((T->parentX == NIL) ? \
			  (T->parentX = (p)) : \
			 (stkPush (T->parentStack, (T->parentX)), \
			  T->parentX = (p))) 

#define rsPutX(x) (T->lastX = (x))
#define rsGetX() (T->lastX)

#define rsInitializeParent(A) ((A)->parentStack = \
			       stkNew (2 * sizeof (int) * 8))

#define rsReleaseParent() (stkDispose (T->parentStack))

#define rsParent(x) (rsGetParent (T, x))

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static void 
rsResetEntry (rsEntryType entry[], int from, int to)

{
  int i;

  for (i = from; i <= to; i++) {
    entry[i].child[0] = NIL;
    entry[i].child[1] = NIL;
  }
}
    
/*--------------------------------------------------------------------------*/

rsType *rsNew (int n,int (*GTfunction)(int,int))

{
  rsType *tmp;
  struct timeval tp;
  struct timezone tzp;

  tmp = MALLOC (rsType, 1);
  tmp->maxn = n;
  tmp->GT = GTfunction;
  tmp->entry = MALLOC (rsEntryType, tmp->maxn + 1);
  rsInitializeParent (tmp);

  gettimeofday(&tp, &tzp);
  srandom(tp.tv_usec % (256*256));

  rsReset (tmp);

  return tmp;
} 

/*--------------------------------------------------------------------------*/

static int rsFindX (rsType *T, int x, int exact)

{
  int act, gt;

  rsResetParent (x);

  act = T->root;

  if (act == NIL) {
    return -1;
  }


  for (T->xGTparent = ((gt = T->GT (x, act)) == 1);
       ((act != x) && (exact || (gt != 0)) &&
	(T->entry[act].child [T->xGTparent] != NIL));
       T->xGTparent = 
        ((gt = T->GT (x, act = T->entry[act].child [T->xGTparent])) == 1)) {
/*    fprintf (stdout,"findX %d %d\n", act, x);*/
    rsPushParent (act);
  }

  if ((act != x) && (exact || (gt != 0))) {
    rsPushParent (act);
    return -1;
  } else {
    if (act != x)
      rsPutX (act);
    return act;
  }
}

/*--------------------------------------------------------------------------*/

static int rsGetPriority (void) 

{
  return (int) (random ());
}

/*--------------------------------------------------------------------------*/

static int rsAppendLeaf (rsType *T, int leaf, int x)

{
  rsEntryType *t;

  t = &(T->entry [leaf]);

  t->child [0] = NIL;
  t->child [1] = NIL;
  t->priority = rsGetPriority ();

  if (x == NIL)
    T->root = leaf;
  else 
    T->entry[x].child [T->xGTparent] = leaf;

  return 0;
}  

/*--------------------------------------------------------------------------*/

static int rsRemoveLeaf (rsType *T, int x, int parentOfX)

{
/*  fprintf (stdout,"removeleaf: %d %d\n", x, parentOfX);*/
  if (parentOfX == NIL)
    T->root = NIL;
  else
    T->entry [parentOfX].child [x == T->entry[parentOfX].child [1]] = NIL;
  T->entry [x].child [0] = NIL;
  T->entry [x].child [1] = NIL;

  return 0;
}  

/*--------------------------------------------------------------------------*/

static void rsRotate (rsType *T, int x, int childOfX, int parentOfX)
     
{
  rsEntryType *t;
  int side, y;
/*  void rsPrintTree ();*/
  
/*  fprintf (stdout,"rsRotate %d %d %d\n", x, childOfX, parentOfX);*/

  if (x == NIL) {
    fprintf (stdout,"ERROR: rsRotate: can't rotate the root.\n");
    exit (1);
  }
  
  t = &(T->entry[x]);
  side = (t->child [1] == childOfX);
  
  y = t->child [side];
  t->child [side] = T->entry[y].child [1 - side];
  T->entry[y].child [1 - side] = x;

  if (parentOfX == NIL)
    T->root = childOfX;
  else
    T->entry[parentOfX].child [T->entry[parentOfX].child [1] == x] = childOfX;

/*  rsPrintTree (T);*/
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

#define T ((rsType *) Tree)

/*--------------------------------------------------------------------------*/

void rsDispose (rsType *Tree)

{
  rsReleaseParent ();
  FREE (T->entry);
  FREE (Tree);
}

/*--------------------------------------------------------------------------*/

void rsReset (rsType *Tree)

{
  T->root = NIL;
  rsResetEntry (T->entry, 0, T->maxn - 1);
}

/*--------------------------------------------------------------------------*/

void rsDelete (rsType *Tree, int x)

{
  int parentOfX, newparent;

  if (rsFindX (T, x, 1) == NIL) {
    rsPrintTree (T);
    fprintf (stdout,"ERROR: rsDelete: element not found %d.\n", x);
    fflush (stdout);
    exit (1);
  }

  parentOfX = rsParent (x);

/*fprintf (stdout,"deleting: %d %d\n", x, rsParent (x));*/

  while ((T->entry[x].child [0] != NIL) || (T->entry[x].child [1] != NIL)) {
    if (T->entry[x].child [0] != NIL)
      if (T->entry[x].child [1] != NIL)
	rsRotate (T, x, newparent = 
		  (T->entry[T->entry[x].child [0]].priority > 
		   T->entry[T->entry[x].child [1]].priority) ?
		  T->entry[x].child [0] : T->entry[x].child [1], 
		  parentOfX);
      else
	rsRotate (T, x, newparent = T->entry[x].child [0], parentOfX);
    else
      rsRotate (T, x, newparent = T->entry[x].child [1], parentOfX);

    parentOfX = newparent;
  }
    
  rsRemoveLeaf (T, x, parentOfX);
}

/*--------------------------------------------------------------------------*/

void rsInsert (rsType *Tree, int x)

{
  int parent;

/*  fprintf (stdout,"inserting %d\n", x);*/

  if (rsFindX (T, x, 1) != NIL) {
    fprintf (stdout,"ERROR: rsInsert: element already in tree.\n");
    exit (1);
  }

  parent = rsParent (x);
  rsAppendLeaf (T, x, parent);

  while ((parent != NIL) && 
	 (T->entry[x].priority > T->entry[parent].priority)) {
    rsRotate (T, parent, x, rsParent (parent));
    parent = rsParent (parent);
  }
}

/*--------------------------------------------------------------------------*/

void rsPrintTree (rsType *Tree)

{
  int i;

  fprintf (stdout,"\nRandsearchtree:\n");
  fprintf (stdout,"maxn %d, root %d\n", T->maxn, T->root);

  for (i = 0; i < T->maxn; i++)
    if ((T->entry[i].child[0] != NIL) || (T->entry[i].child[1] != NIL)) {
      fprintf (stdout,"%d", i);
      if (T->entry[i].child[0] != NIL) {
	fprintf (stdout,"    %d", T->entry[i].child[0]);
	if (T->GT (i, T->entry[i].child[0]) != 1) {
	  fprintf (stdout,"\nERROR: rsPrintTree: tree inconsistent %d %d\n", T->GT (i, T->entry[i].child[0]), T->GT (T->entry[i].child[0], i)); fflush (stdout);
	  exit (1);
	}
      } else 
	fprintf (stdout,"    NIL");
      if (T->entry[i].child[1] != NIL) {
        fprintf (stdout,"    %d\n", T->entry[i].child[1]);
	if (T->GT (i, T->entry[i].child[1]) != -1) {
	  fprintf (stdout,"\nERROR: rsPrintTree: tree inconsistent %d %d\n", T->GT (i, T->entry[i].child[1]), T->GT (T->entry[i].child[1], i)); fflush (stdout);
	  exit (1);
	}
      } else 
        fprintf (stdout,"    NIL\n");
    }

  fprintf (stdout,"\n");
}

/*--------------------------------------------------------------------------*/

int rsLargest (rsType *Tree)

{
  int x;

  rsResetParent (NIL);

  x = T->root;

  if (x == NIL)
    return NIL;

  while (T->entry[x].child [1] != NIL) {
    rsPushParent (x);
    x = T->entry[x].child [1];
  }

  rsPutX (x);

  return x;
}

/*--------------------------------------------------------------------------*/

int rsSmallest (rsType *Tree)

{
  int x;

  rsResetParent (NIL);

  x = T->root;

  if (x == NIL)
    return NIL;

  while (T->entry[x].child [0] != NIL) {
    rsPushParent (x);
    x = T->entry[x].child [0];
  }

  rsPutX (x);

  return x;
}

/*--------------------------------------------------------------------------*/

int rsNextSmaller (rsType *Tree)

{
  int x, xOrig;

  xOrig = x = rsGetX ();

  if (T->xFound) {

    if (T->entry[x].child[0] != NIL) {
      rsPushParent (x);
      x = T->entry[x].child[0];


      while (T->entry[x].child[1] != NIL) {
	rsPushParent (x);
	x = T->entry[x].child[1];
      }
      
      rsPutX (x);
      return x;
    } 

  } else 

    if (rsParent (x) != NIL) {
      T->xFound = 1;
      (void) rsParent (x = rsParent (x));

      if (T->xGTparent)
	return x;
    } else
      return NIL;

  while ((rsParent (x) != NIL) && (T->entry[rsParent (x)].child [0] == x))
    x = rsParent (x);

  if (rsParent (x) != NIL) {
    (void) rsParent (x = rsParent (x));
    return x;
  } else {
    rsSmallest (T);
    if (xOrig != rsGetX ()) {
      rsPushParent (rsGetX ());
      rsPutX (xOrig);
      T->xFound = 0;
    }
    return NIL;
  }
}

/*--------------------------------------------------------------------------*/

int rsNextLarger (rsType *Tree)

{
  int x, xOrig;

  xOrig = x = rsGetX ();

/*rsPrintTree (Tree);*/

  if (T->xFound) {

    if (T->entry[x].child[1] != NIL) {
      rsPushParent (x);
      x = T->entry[x].child[1];

      while (T->entry[x].child[0] != NIL) {
	rsPushParent (x);
	x = T->entry[x].child[0];
      }

      rsPutX (x);
      return x;
    } 

  } else 

    if (rsParent (x) != NIL) {
      T->xFound = 1;
      (void) rsParent (x = rsParent (x));

      if (! T->xGTparent)
	return x;
    } else
      return NIL;

  while ((rsParent (x) != NIL) && (T->entry[rsParent (x)].child [1] == x)) {
    x = rsParent (x);
  }

  if (rsParent (x) != NIL) {
    (void) rsParent (x = rsParent (x));
    return x;
  } else {
    rsLargest (T);
    if (xOrig != rsGetX ()) {
      rsPushParent (rsGetX ());
      rsPutX (xOrig);
      T->xFound = 0;
    }

    return NIL;
  }
}

/*--------------------------------------------------------------------------*/

int rsFindInit (rsType *Tree, int x)

{
  int tmp;
  
  tmp = rsFindX (T, x, 0);
  T->xFound = (tmp != NIL);

  return (tmp);
}

/*--------------------------------------------------------------------------*/

static int rsGT (int a, int b)

{
  return (a > b ? 1 : -1);
}

/*--------------------------------------------------------------------------*/

int testrand (void) 

{
  void *rs;

  rs = rsNew (100, rsGT);

  fprintf (stdout,"                  smaller: %d %d\n", 23, rsNextSmaller (rs));
  fprintf (stdout,"                  larger:  %d %d\n",23, rsNextLarger (rs));

  rsInsert (rs, 10);
  rsPrintTree (rs);
  rsInsert (rs, 40);
  rsPrintTree (rs);

  rsFindInit (rs, 9);
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));

  rsInsert (rs, 23);
  rsPrintTree (rs);
  rsInsert (rs, 60);
  rsInsert (rs, 25);
  rsInsert (rs, 20);
  rsPrintTree (rs);

  rsInsert (rs, 19);
  rsPrintTree (rs);

  rsFindInit (rs, 18);
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  rsFindInit (rs, 70);
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  smaller: %d\n", rsNextSmaller (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));
  fprintf (stdout,"                  larger:  %d\n", rsNextLarger (rs));

  rsDelete (rs, 23);
  rsPrintTree (rs);
  rsDelete (rs, 10);
  rsPrintTree (rs);
  rsDelete (rs, 19);
  rsPrintTree (rs);

  rsDispose (rs);

  return 0;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
