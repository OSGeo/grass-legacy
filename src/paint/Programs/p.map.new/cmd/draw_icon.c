#include "graphics.h"
#include "sites.h"

int draw_icon (ICON *icon, int x, int y)
{
    int row, col;

    y -= icon->yref;
    x -= icon->xref;

    for (row = 0; row < icon->nrows; row++)
	for (col = 0; col < icon->ncols; col++)
	    if (icon->map[row][col])
		dot(x+col,y+row);

    return 0;
}
