/************************************************************
* 	rclmap()                                            *
* takes as input a column number                           *
* the subroutine the obtains the column number of that     *
* column's receiving cell and stores that in               *
* columndata[column_number]->receiving_cell_position       *
************************************************************/
 
 
 
#include "input.h"
#include "stdio.h"
#include "debugflg.h"

extern int columns;
extern COLUMN_INFOPTR *columndata;
extern FLAGS_ROUTINE   rflags;
 
#ifdef _UNIX_K_AND_R
 
 extern int search();
 
#else
 
 extern int search(long rec_cell);
 
#endif
 
 
#ifdef _UNIX_K_AND_R
 
 void rclmap(column_number)
 
    int column_number;
 
#else
 
 void rclmap(column_number)
 
#endif
{
int  search_flag;
long rec_cell;
long max_source_cell;
int mid;
 
 
 
 
/*** 1.1.1 CALCULATE RECEIVING CELL NUMBER ******/
 
  rec_cell = columndata[column_number]->receiving_cell_number * 1000L +
		columndata[column_number]->receiving_cell_division;
 
 
 
  search_flag = FALSE;
 
 
 
 
 
/*** 1.1.2 MARK SINKHOLES ***/
 
  /* if cell is a sink hole label the cell with a 0 */
 
  if(rec_cell == 0L)
     {
     columndata[column_number]->receiving_cell_position = 0;
     search_flag = TRUE;
     }
 
 


/*** 1.1.3 CALCULATE MAXIMUM CELL NUMBER ***/

  max_source_cell = columndata[columns]->cell_number * 1000L +
		columndata[columns]->cell_division;




/*** 1.1.4 MARK OUTLET CELL ***/

  /* label the outlet cell 1 greater than the max cell number */

  if((search_flag == FALSE) && (rec_cell > max_source_cell))
     {
     columndata[column_number]->receiving_cell_position = columns + 1;
     search_flag = TRUE;
     }




/*** 1.1.5 SEARCH FOR RECEIVING CELL ***/

  if(search_flag == FALSE)
     {
     mid = search(rec_cell);
     if (mid > 0)
	columndata[column_number]->receiving_cell_position = mid;
     }

  if (rflags.rclmap)
   {
    fprintf (stderr,"RCLMAP routine: \n");
    fprintf (stderr,"   Input:  %d...col_number\n",column_number);
    fprintf (stderr,"   Output: %d...rec_cell_position\n",columndata[column_number]->receiving_cell_position);
   }
  return;
}
