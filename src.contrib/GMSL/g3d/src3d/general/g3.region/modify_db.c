#include <stdio.h>
#include "gis.h"
#include "glob.h"
#include "G3d.h"
#include "local_proto.h"
int
modify_db()
{
    G3D_Region window ;
    char name[30];

    if (!G_ask_in_mapset_ext("Enter name of 3d region to be modified",
	name, G3D_WINDOW_DATABASE, "3d region", "with region values", lister))
	    return 1;

    if (! G3d_readWindow (&window, name))
	fprintf (stderr, "region [%s] in mapset [%s]. can't select.\n",
		name, G_mapset ());
    if (!edit_window (&window))
	return 1;
    G3d_setWindow (&window);
    if (! G3d_writeWindow (&window, name))
    {
	fprintf (stderr, "** can't write region [%s]. ", name) ;
	return 0;
    }
    fprintf (stderr, "region [%s] modified.\n", name) ;

    return 1;
}
