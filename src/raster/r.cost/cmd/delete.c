/***************************************************************/
/*                                                             */
/*     delete.c           in                ~/src/Gcost        */
/*                                                             */
/*     This routine searches, starting from the end of the     */
/*     linked list, for the cells to be removed from the       */
/*     list. Then it updates the adjacent pointers and         */
/*      frees the memory previously allocated to the cell.     */
/*                                                             */
/***************************************************************/

#include "gis.h"
#include "cost.h"

delete(end_cell, row, col)
     struct cost **end_cell;
     int row, col;

{
  struct cost *pres_cell;

  pres_cell = *end_cell;
  while(1)
    {
      if((pres_cell->row == row) && (pres_cell->col == col))
	break;

      pres_cell = pres_cell->prev;
    }

  pres_cell->prev->next = pres_cell->next;

  if(pres_cell->next == NULL)
    *end_cell = pres_cell->prev;
  else
    pres_cell->next->prev = pres_cell->prev;

  free(pres_cell);

  return; 
}

/****************** END OF FUNCTION DELETE *********************/  
