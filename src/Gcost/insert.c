/***************************************************************//*                                                             *//*      insert.c           in         ~/src/Gcost              *//*                                                             *//*    This routine creates a doubly linked list of cell        *//*    data structures. It allocates memory for the cell to     */
/*    be inserted, assigns given values to the structure       */
/*    attributes, then it inserts the new cell in the list     */
/*    by increasing costs.                                     *//*                                                             *//***************************************************************/

#include "gis.h"
#include "cost.h"

struct cost *insert(start_cell, end_cell, min_cost, row, col)
     struct cost **start_cell, **end_cell;
     int row, col;
     float min_cost;

{
  char *malloc();
  struct cost *new_cell, *pres_cell, *ptr_end_cell;

  new_cell = (struct cost *)(malloc(sizeof(struct cost)));

  new_cell->min_cost = min_cost;
  new_cell->row = row;
  new_cell->col = col;

  if (*start_cell == NULL) 
    {
      *start_cell = new_cell;
      *end_cell = new_cell;
      new_cell->next = NULL;
      new_cell->prev = NULL;

      return(new_cell);
    }

  ptr_end_cell = *end_cell;

  if(min_cost >= ptr_end_cell->min_cost)
    {
      ptr_end_cell->next = new_cell;
      new_cell->prev = *end_cell;
      new_cell->next = NULL;
      *end_cell = new_cell;

      return(new_cell);
    }

  pres_cell = *end_cell;

/*Insert new cell starting from start of linked list */

  while (pres_cell->prev != NULL) 
    {
      if (pres_cell->prev->min_cost <= min_cost)
	{
	  pres_cell->prev->next = new_cell;
	  new_cell->prev = pres_cell->prev;
	  new_cell->next = pres_cell;
	  pres_cell->prev = new_cell;

	  return(new_cell);
	}
      pres_cell = pres_cell->prev;

    }

  *start_cell = new_cell;
  pres_cell->prev = new_cell;
  new_cell->next = pres_cell;
  new_cell->prev = NULL;

  return(new_cell);
}

/**************** END OF FUNCTION "INSERT" *********************/
 

     

