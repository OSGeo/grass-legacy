
/* 
 * FILE: elist.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 * The functions in the file implements an edge-list for
 * the polygon-fill algorithm polyapply.c and ilist.c.  The
 * edge-list is actually a stack implemented by use of the 
 * the generic linked list functions used in llist.c.  
 *
 * ELinit(xarr,yarr,ppoints) 
 * -------------------------
 * This function takes a polygon whose points are stored in
 * the arrays xarr and yarr and builds an edge-list.
 *
 * ELyMax() 
 * --------
 * This function returns the maximum y coordinate value in 
 * in edge-list.
 *
 * ELyMin() 
 * --------
 * This function returns the minimum y coordinate value in 
 * in edge-list.
 *
 * ELfree() 
 * --------
 * This function frees all memory associated with an edgelist.
 *
 */

#include <stdio.h>
#include <malloc.h>
#include <values.h>   /* FIX: Sun specific? */
#include "ll.h"
#include "el.h"

/**************/
/*** ELinit ***/
/**************/

elist 
#ifdef _NO_PROTO
*ELinit(xarr,yarr,ppoints)
   int xarr[];
   int yarr[];
   int ppoints;
#else
*ELinit(
   int xarr[],
   int yarr[],
   int ppoints)
#endif
{
int ip;
Edge *edge;
elist *edgelist;

#ifdef DEBUG
printf("ELinit: begin\n");
#endif

/* allocate space for edgelist */
edgelist = (elist *)malloc(sizeof(elist));
edgelist->list = LLinit();
edgelist->ymax = 0;
edgelist->ymin = MAXINT;

/* loop through polygon points to build edgelist */
for (ip = 0; ip < ppoints; ip++)
   {
   edge = (Edge *)malloc(sizeof(Edge));
   edge->x1 = xarr[ip];
   edge->y1 = yarr[ip];
   if (yarr[ip] > edgelist->ymax) edgelist->ymax = yarr[ip];
   if (yarr[ip] < edgelist->ymin) edgelist->ymin = yarr[ip];
   if (ip == ppoints-1)
      {
      /* wrap around to first in list */
      edge->x2 = xarr[0];
      edge->y2 = yarr[0];
      }
   else
      {
      edge->x2 = xarr[ip+1];
      edge->y2 = yarr[ip+1];
      }
   /* figure max and min y values of edge */
   edge->ymax = (edge->y1 > edge->y2) ? edge->y1 : edge->y2;
   edge->ymin = (edge->y1 < edge->y2) ? edge->y1 : edge->y2;

   if (!(edge->x1 == edge->x2 && edge->y1 == edge->y2))
      {
#     ifdef DEBUG
      printf("Edge (%d,%d) (%d,%d)\n",edge->x1,edge->y1,edge->x2,edge->y2);
#     endif 
      LLpushEnd(edgelist->list,(unsigned char *)edge);
      }
   }
#ifdef DEBUG
printf("ELinit: end\n");
#endif

return(edgelist);
}

/**************/
/*** ELyMax ***/
/**************/

int 
#ifdef _NO_PROTO
ELyMax(edgelist)
   elist *edgelist;
#else
ELyMax(elist *edgelist)
#endif
{
return(edgelist->ymax);
}

/**************/
/*** ELyMin ***/
/**************/

int
#ifdef _NO_PROTO
ELyMin(edgelist)
   elist *edgelist;
#else
ELyMin(elist *edgelist)
#endif
{
return(edgelist->ymin);
}

/**************/
/*** ELfree ***/
/**************/

void 
#ifdef _NO_PROTO
ELfree(edgelist)
   elist *edgelist;
#else
ELfree(elist *edgelist)
#endif
{
#ifdef DEBUG1
printf("ELfree: begin\n");
#endif

if (edgelist->list != NULL)
   LLfreeList(edgelist->list);
if (edgelist != NULL) 
   free(edgelist);

#ifdef DEBUG1
printf("ELfree: end\n");
#endif
}

