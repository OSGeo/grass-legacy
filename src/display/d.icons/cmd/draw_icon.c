#include "icon.h"
#include "display.h"

int draw_icon (ICON *icon, register int x, register int y)
{
    register int row, col;
    register int cur, prev;

    y -= icon->yref;
    x -= icon->xref;

    for (row = 0; row < icon->nrows; row++)
    {
	prev = 0;
	for (col = 0; col < icon->ncols; col++)
	{
	    cur = icon->map[row][col];
	    if (cur != prev)
	    {
		if (prev)
		    D_cont_abs(x+col-1,y+row);
		else
		    D_move_abs(x+col,y+row);
		prev = cur;
	    }
	}
	if (prev)
	    D_cont_abs(x+col-1,y+row);
    }

    return 0;
}
