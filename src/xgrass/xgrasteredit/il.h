
/*
 * FILE: il.h
 * 
 * PROGRAMMER: David M. Johnson
 *
 * Types and function prototypes for the Intersection List 
 * functions in ilist.c
 *
 */

/*******************************/
/*** INTERSECTION LIST TYPES ***/
/*******************************/

typedef llist ilist;

typedef struct _Intersect
   {
   int x;
   } Intersect;

/*********************************************/
/*** INTERSECTION LIST FUNCTION PROTOTYPES ***/
/*********************************************/

int IntersectCompare(
#ifndef _NO_PROTO
   Intersect *int1,
   Intersect *int2
#endif
);

ilist *ILinit(
#ifndef _NO_PROTO
   int iy,
   elist *edgelist
#endif
);

void ILfree(
#ifndef _NO_PROTO
   ilist *intlist
#endif
);

Intersect *ILpop(
#ifndef _NO_PROTO
   ilist *intlist;
#endif
);

