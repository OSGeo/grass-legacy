
/*
 * FILE: polyapply.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTION:
 *
 * PolygonApplyBrush(xarr,yarr,ppoints)
 * ------------------------------------
 * This function applies the global brush to each raster map 
 * cell within the polygon defined by the arrays xarr and yarr
 * (ppoints is the number of points in the polygon).  The 
 * algorithm is based on the polygon fill algorithm in 
 * Fundamentals of Interactive Computer Graphics by Filey 
 * and Van Dam on pages 456-460.  
 *
 */

#include "xgre.h"
#include "ll.h"
#include "el.h"
#include "il.h"

/*************************/
/*** PolygonApplyBrush ***/
/*************************/

void PolygonApplyBrush(xarr,yarr,ppoints)
int xarr[]; 
int yarr[]; 
int ppoints;
{
int ix,iy;
int ymin, ymax;
elist *edgelist;
ilist *intlist;
Intersect *int1;
Intersect *int2;
llnode *lptr;

#ifdef DEBUG
printf("PolyApply:\n");
#endif

/* build a list of all the polygon edges */ 
edgelist = (elist *)ELinit(xarr,yarr,ppoints);

/* get minimum and maximum y value in polygon */ 
ymin = ELyMin(edgelist);
ymax = ELyMax(edgelist);

/* loop through each "scan-line" in the y range of polygon */ 
for (iy = ymin; iy <= ymax; iy++)
   {
   int count=0;

   /* build a sorted list of all of the intersections of
    * the current scan-line and the edges of the polygon
    */
   intlist = (ilist *)ILinit(iy,edgelist);
#  ifdef DEBUG
   printf("PolyApply: list ready count=%d\n",intlist->count);
#  endif 

   /* pop two intersections from intersection list, and 
    * fill between them 
    */
   while((int1=ILpop(intlist))!=NULL && (int2=ILpop(intlist))!=NULL)
      {
#     ifdef DEBUG
      printf("PolyApply: x1=%d x2=%d\n",int1->x,int2->x);
#     endif 
      for (ix = int1->x; ix <= int2->x; ix++) ApplyBrush(ix,iy);
      if (int1 != NULL) free(int1);
      if (int2 != NULL) free(int2);
      if (intlist->count == 0) break;
      }
   ILfree(intlist);
   }

ELfree(edgelist);
ClearUndoBuffer();
}

