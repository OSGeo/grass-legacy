
/* 
 * FILE: ilist.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 * 
 * These functions implement a sorted intersection-list for
 * the polygon-fill function in polyapply.c.  The intersection
 * type (Intersect) and the intersection-list type (ilist) are 
 * defined in il.h.  The polygon edge-list type and functions 
 * are defined in el.h and elist.c.  These functions also use 
 * the linked-list type and functions defined in  ll.h and 
 * llist.c.
 * 
 * ilist *ILinit(int iy,edgelist)
 * ------------------------------  
 * This function is used in the polygon-fill algorithm 
 * to build an intersection list for one horizontal 
 * scan-line (iy).  This function checks each edge in 
 * the polygon (edgelist) to see if it intersects the 
 * scan-line.  If it does intersect then the intersection 
 * is added to the ordered intersection list.  If the 
 * intersection occurs on a vertex that is a local minimum, 
 * then it is inserted into the list twice, otherwise
 * it is inserted once.
 *
 * FIX: There are a couple cases for which this function 
 * will not build a good intersection list.  These could 
 * probably be fixed by implementing the "edge shortening" 
 * idea discussed in Foley and Van Dam on page 455.  The
 * user can avoid these cases by NOT attempting to close
 * the polygon perfectly by entering a final point close
 * to or right-on the beginning point.
 *
 * Intersect *ILpop(ilist *intlist)
 * --------------------------------
 * Pop the first intersection (from the beginning) of the 
 * intersection list pointed to by intlist and return a 
 * pointer to it.
 *
 * ILfree(ilist *intlist)  
 * ----------------------
 * Destroy the intersection list pointed to by intlist and
 * free all memory associated with it. 
 *
 */

#include <stdio.h>
#include <malloc.h>
#include <values.h>   /* FIX: Sun specific? */
#include "ll.h"       /* linked-list header */
#include "el.h"       /* edge-list header */
#include "il.h"       /* intersection-list header */

/************************/
/*** IntersectCompare ***/
/************************/

int 
#ifdef _NO_PROTO
IntersectCompare(int1,int2)
   Intersect *int1;
   Intersect *int2;
#else
IntersectCompare(Intersect *int1, Intersect *int2)
#endif
{
if      (int1->x > int2->x) return(LL_GREATER_THAN);
else if (int1->x < int2->x) return(LL_LESS_THAN);
else                        return(LL_EQUAL);
}

/**************/
/*** ILinit ***/
/**************/

ilist 
#ifdef _NO_PROTO
*ILinit(iy,edgelist)
   int iy;
   elist *edgelist;
#else
*ILinit(int iy, elist *edgelist)
#endif
{
ilist *intlist;
Intersect *int1;
Intersect *int2;
llnode *lptr;
Edge *eptr, *prevedge, *nextedge;

#ifdef DEBUG
printf("*** ILinit ***\n");
#endif

if ((lptr = edgelist->list->begin) == NULL) return(NULL);

intlist = (ilist *)LLinit();

/*** LOOP THROUGH EDGES ***/
while (lptr != NULL)
   {
   eptr = (Edge *)lptr->data;
   if (!(iy > eptr->ymax || iy < eptr->ymin) &&   /* if in yrange */
       !(eptr->y1 == eptr->y2))                   /* and not horizontal */
      {
      /*** THEN THERE IS AN INTERSECTION ***/
      int i1;
      if (iy == eptr->y1)
         {
         /*** WITH EDGE POINT 1 ***/
#        ifdef DEBUG
         printf("vertex1 ");
#        endif 
         i1 = eptr->x1;
         }
      else if (iy == eptr->y2)
         {
         /*** WITH EDGE POINT 2 ***/
#        ifdef DEBUG
         printf("vertex2 ");
#        endif 
         i1 = eptr->x2;
         }
      else if (eptr->x1 == eptr->x2)
         {
         /*** WITH A VERTICAL LINE ***/
#        ifdef DEBUG
         printf("vertical ");
#        endif 
         i1 = eptr->x1;
         }
      /*else if (eptr->y1 == eptr->y2) 
         {
#        ifdef DEBUG
         printf("horizontal ");
#        endif 
         i1 = (eptr->x1 < eptr->x2) ? eptr->x1 : eptr->x2;
         }*/
      else
         { 
         /*** WITH A DIAGONAL LINE ***/
         float m = (float)(eptr->y2-eptr->y1) / (float)(eptr->x2-eptr->x1);
         float b = (float)eptr->y1 - m*(float)eptr->x1; 
#        ifdef DEBUG
         printf("diagonal ");
#        endif 
         i1 = ((float)iy - b) / m;
         }
#     ifdef DEBUG
      printf("(%d,%d) ",i1,iy);
#     endif 

      /*** INSERT INTERSECTION INTO ORDERED INTERSECTION LIST ***/

      /* point to next edge */
      if (lptr->next == NULL) nextedge = (Edge*)edgelist->list->begin->data; 
      else                    nextedge = (Edge*)lptr->next->data; 

      /* point to previous edge */
      if (lptr->prev == NULL) prevedge = (Edge*)edgelist->list->end->data;   
      else                    prevedge = (Edge*)lptr->prev->data;      

      if (iy == eptr->y1)
         {
         /* intersection matches 1st edge vertex*/
         if ((eptr->y1 < prevedge->y1 && eptr->y1 < eptr->y2) || 
             (eptr->y1 > prevedge->y1 && eptr->y1 > eptr->y2))
            {
            /* intersection is a local min or max */
#           ifdef DEBUG
            printf("local min/max");
#           endif 
            int1 = (Intersect *)malloc(sizeof(Intersect));
            int1->x = i1;
            LLinsertWithDups(intlist,(unsigned char*)int1,IntersectCompare);
            }
         else
            {
            /* not local min or max */
            int1 = (Intersect *)malloc(sizeof(Intersect));
            int1->x = i1;
            LLinsertNoDups(intlist,(unsigned char*)int1,IntersectCompare);
            }
         }
      else if (iy == eptr->y2)
         {
         /* intersection matches 2nd edge vertex */
         if ((eptr->y2 < nextedge->y2 && eptr->y2 < eptr->y1) ||
             (eptr->y2 > nextedge->y2 && eptr->y2 > eptr->y1))
            {
#           ifdef DEBUG
            printf("local min/max");
#           endif 
            /* intersection is a local min or max so insert it twice */
            int2 = (Intersect *)malloc(sizeof(Intersect));
            int2->x = i1;
            LLinsertWithDups(intlist,(unsigned char*)int2,IntersectCompare);
            }
         else
            {
            /* not a local min or max */
            int1 = (Intersect *)malloc(sizeof(Intersect));
            int1->x = i1;
            LLinsertNoDups(intlist,(unsigned char*)int1,IntersectCompare);
            }
         }
      else
         {
         /* intersection no on a vertex */
         int1 = (Intersect *)malloc(sizeof(Intersect));
         int1->x = i1;
         LLinsertWithDups(intlist,(unsigned char*)int1,IntersectCompare);
         }
      }
   lptr = lptr->next;
   }
return(intlist);
}

/*************/
/*** ILpop ***/
/*************/

Intersect 
#ifdef _NO_PROTO
*ILpop(intlist)
   ilist *intlist;
#else
*ILpop(ilist *intlist)
#endif
{
return((Intersect*)LLpopBegin((llist*)intlist));
}

/**************/
/*** ILfree ***/
/**************/

void 
#ifdef _NO_PROTO
ILfree(intlist)
   ilist *intlist;
#else
ILfree(ilist *intlist)
#endif
{
#ifdef DEBUG1
printf("ILfree: begin\n");
#endif

LLfreeList((llist*)intlist);

#ifdef DEBUG1
printf("ILfree: end\n");
#endif
}

