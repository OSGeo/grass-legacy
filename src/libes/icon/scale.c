#include "gis.h"
#include "icon.h"

scale_icon (old, new, scale)
    ICON *old, *new;
    float scale;
{
    register int row, col;
    register int r,c;

    if (scale <= 0.0) scale = 1.0;
    else if (scale < .5) scale = .5;

    new->nrows = old->nrows * scale;
    new->ncols = old->ncols * scale;

    if (new->nrows <= 0 || new->ncols <= 0)
    {
	new->ncols = 1;
	new->nrows = 1;
    }

    new->map = (char **) G_malloc (new->nrows * sizeof (char *));
    for (row = 0; row < new->nrows; row++)
	G_zero (new->map[row] = G_malloc (new->ncols), new->ncols);

    for (row = 0; row < new->nrows; row++)
    {
	r = row/scale;
	if (r >= old->nrows) break;

	for (col = 0; col < new->ncols; col++)
	{
	    c = col/scale;
	    if (c >= old->ncols) break;

	    new->map[row][col] = old->map[r][c];
	}
    }

    new->xref = old->xref * scale;
    new->yref = old->yref * scale;
}
