#include "gis.h"
#include "icon.h"

#define X (char)1
#define b (char)0
#define NROWS 7
#define NCOLS 7

get_default_icon (icon)
    ICON *icon;
{
    static char def_icon[NROWS][NCOLS] =
    {
	{b,b,b,X,b,b,b},
	{b,b,X,X,X,b,b},
	{b,X,X,X,X,X,b},
	{X,X,X,X,X,X,X},
	{b,X,X,X,X,X,b},
	{b,b,X,X,X,b,b},
	{b,b,b,X,b,b,b}
    };

    int row;
    int col;

    icon->nrows = NROWS ;
    icon->ncols = NCOLS ;
    icon->yref = NROWS/2;
    icon->xref = NCOLS/2;

    icon->map = (char **) G_malloc (NROWS * sizeof (char *));
    for (row = 0; row < NROWS; row++)
    {
	icon->map[row] = G_malloc (NCOLS);
	for (col = 0; col < NCOLS; col++)
	    icon->map[row][col] = def_icon[row][col];
    }
}
