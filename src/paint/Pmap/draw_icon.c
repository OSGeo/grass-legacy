#include "icon.h"
draw_icon (icon, x, y)
    ICON *icon;
{
    int row, col;

    y -= icon->yref;
    x -= icon->xref;

    for (row = 0; row < icon->nrows; row++)
	for (col = 0; col < icon->ncols; col++)
	    if (icon->map[row][col])
		dot(x+col,y+row);
}
