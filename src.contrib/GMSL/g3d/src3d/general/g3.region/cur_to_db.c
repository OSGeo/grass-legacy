#include "glob.h"
#include "G3d.h"
cur_to_db()
{
    char name[30];
    char *mapset;

    mapset = G_ask_new_ext ("Enter a name for the current 3d region",
	name,G3D_WINDOW_DATABASE,"3d region","with region values", lister);
    if (!mapset) return 1;

    if (! G3d_writeWindow (G3d_windowPtr (), name) < 0)
	fprintf (stderr, "** can't save region [%s]. ", name);
    else
	fprintf (stderr, "current region saved as [%s]. ", name);
    return 0;
}
