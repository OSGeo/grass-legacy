#include <stdlib.h>
#include "icon.h"

int release_icon (ICON *icon)
{
    int row;

    if (icon->nrows > 0)
    {
	for (row = 0; row < icon->nrows; row++)
	    free (icon->map[row]);
	free (icon->map);
    }
    icon->nrows = 0;

    return 0;
}
