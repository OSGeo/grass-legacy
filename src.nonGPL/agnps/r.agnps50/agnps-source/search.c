/************************************************************
*     search()                                             *
* This subroutine does a binary search on the columndata   *
* array to find the column number that matches the         *
* receiving cell's code.                                   *
************************************************************/
 
#ifdef _DOS
 
 #include <stdio.h>
 #include <stdlib.h>
 #include <conio.h>
 #include "input.h"
 #include "debugflg.h"

#else
 
#include <stdio.h>
#include "input.h"
#include "debugflg.h"

#endif
 
extern int columns;
extern COLUMN_INFOPTR *columndata;
 
#ifdef _UNIX_K_AND_R
 
 int search(rec_cell)
 
   long rec_cell;
 
#else
 
 int search(long rec_cell)
 
#endif
 
{
int problem_flag  = FALSE;
int found_flag    = FALSE;
 
int mid  = 0;
int lo   = 1;
int hi   = columns;
long source_cell;          /* These three are codes for cell number  */
long source_cell_hi;       /* and cell division                      */
long source_cell_lo;
 
 
 
/*** 1.1.5.1  CALCULATE MAXIMUM CELL NUMBER ***/
 
  source_cell_hi = (long) (columndata[hi]->cell_number) * 1000L +
		   (long) (columndata[hi]->cell_division);
 
 
 
 
/*** 1.1.5.2  CALCULATE MINIMUM CELL NUMBER ***/
 
  source_cell_lo = (long) (columndata[lo]->cell_number) * 1000L +
		   (long) (columndata[lo]->cell_division);
 
 
 
 
/*** 1.1.5.3  MARK UNSORTED LIST ***/
 
  if((rec_cell > source_cell_hi) || (rec_cell < source_cell_lo))
     problem_flag = TRUE;
 
 
 
 
 
/*** 1.1.5.4  SEARCH FOR THE RECEVING CELL POSITION ***/
 
  /* This is just doing a binary search for the receiving cell */
 
  while( (lo <= hi) && (found_flag == FALSE) && (problem_flag == FALSE) )
     {
     mid = (lo+hi)/2;
     source_cell = (long) (columndata[mid]->cell_number) * 1000L +
		   (long) (columndata[mid]->cell_division);
 
     if(rec_cell == source_cell)
       found_flag = TRUE;
     else
       {
       if(rec_cell < source_cell)
	  hi = mid - 1;
       else
	  lo = mid + 1;
       }
     }
 
 
 
 
/*** 1.1.5.5.  MARK CELL NOT FOUND ***/
 
  if (lo > hi)
      fprintf (stderr,"PROBLEM error or cannot find source cell");
 
 
  if( problem_flag )
      fprintf (stderr,"PROBLEM");
 
 
 
  return( mid );
}
