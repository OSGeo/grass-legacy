
/* 
 * FILE: el.h
 *
 * PROGRAMMER: David M. Johnson
 *
 */

/*** EDGE LIST TYPES ***/

typedef struct _elist    /* POLYGON EDGE LIST */
   {
   llist *list;          /* linked-list of edges */
   int ymax;             /* maximum y coordinate value */
   int ymin;             /* minimum y coordinate value */
   } elist;

typedef struct _Edge     /* ONE POLYGON EDGE */
   {
   int x1;               /* point 1 */
   int y1;
   int x2;               /* point 2 */
   int y2;
   int ymax;
   int ymin;
   } Edge;

/*** EDGE LIST FUNCTION PROTOTYPES ***/

elist *ELinit(
#ifndef _NO_PROTO
   int xarr[],
   int yarr[],
   int ppoints
#endif
);

int ELyMin(
#ifndef _NO_PROTO
   elist *edgelist;
#endif
);

int ELyMax(
#ifndef _NO_PROTO
   elist *edgelist;
#endif
);

void ELfree(
#ifndef _NO_PROTO
   elist *edgelist;
#endif
);

