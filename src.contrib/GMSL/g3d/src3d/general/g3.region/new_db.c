#include <stdio.h>
#include "gis.h"
#include "glob.h"
#include "G3d.h"
#include "local_proto.h"
int
new_db()
{
    G3D_Region window ;
    char name[30];
    char fullName[1000];

    if (!G_ask_new_ext("Enter name for region to be created",
	name, G3D_WINDOW_DATABASE, "region", "with region values", lister))
	    return 1;

    
    G__file_name (fullName, "", G3D_DEFAULT_WINDOW_ELEMENT, 
		  G3D_PERMANENT_MAPSET);
    if (! (G3d_readWindow (&window, fullName)))
      fprintf (stderr, "region [%s] in mapset [%s]. can't select.\n",
	       G3D_DEFAULT_WINDOW_ELEMENT, G3D_PERMANENT_MAPSET);
    G3d_readWindow (&window, fullName);

    if(!edit_window (&window)) return 1;
    G3d_setWindow (&window);
    if (! G3d_writeWindow (&window, name))
    {
	fprintf (stderr, "** can't write region [%s]. ", name) ;
	return 0;
    }
    fprintf (stderr, "region [%s] created.\n", name) ;
    return 1;
}
