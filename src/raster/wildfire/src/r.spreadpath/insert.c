#include "gis.h"
#include "segment.h"
#include "point.h"

insert (pres_pt, row, col, backrow, backcol)
POINT **pres_pt;
int row, col, backrow, backcol;
{
	extern POINT  *head_start_pt;
	POINT *new_pt;

	new_pt = (struct point *) (malloc (sizeof (struct point)));

	new_pt->row = row;
	new_pt->col = col;
	new_pt->backrow = backrow;
	new_pt->backcol = backcol;
	new_pt->next= NULL;

	if (head_start_pt == NULL) {
	head_start_pt = new_pt;
	*pres_pt = head_start_pt;
	}
	else 
	{
		(*pres_pt)->next = new_pt;
		*pres_pt = (*pres_pt)->next;
	}
}
		
