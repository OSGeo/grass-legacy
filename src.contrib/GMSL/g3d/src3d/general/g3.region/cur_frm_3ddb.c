#include "glob.h"
#include "G3d.h"
cur_from_3ddb()
{
    char name[30], fullName[100];
    char *mapset;
    G3D_Region window;

    mapset = G_ask_old_ext ("Select a 3d region",
			    name, G3D_WINDOW_DATABASE, "3d region",
			    "with region values", lister);
    if (!mapset) return 1;

    sprintf (fullName, "%s@%s", name, mapset);
    if (! G3d_readWindow (&window, fullName))
	fprintf (stderr, "region [%s] in mapset [%s]. can't select.\n",
		name, mapset);
    else
    {
	if(!edit_window(&window)) return 1;
	set_window (&window, name);
    }
    return 0;
}
