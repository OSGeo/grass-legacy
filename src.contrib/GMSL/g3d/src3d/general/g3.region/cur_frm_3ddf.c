#include "glob.h"
#include "G3d.h"
#include "local_proto.h"
int
cur_from_3ddef()
{
    char fullName[1000];
    G3D_Region window;
    
    G__file_name (fullName, "", G3D_DEFAULT_WINDOW_ELEMENT, 
		  G3D_PERMANENT_MAPSET);
    if (! (G3d_readWindow (&window, fullName)))
	fprintf (stderr, "region [%s] in mapset [%s]. can't select.\n",
		 G3D_DEFAULT_WINDOW_ELEMENT, G3D_PERMANENT_MAPSET);
    if(!edit_window(&window)) return 1;
    set_window (&window, "default 3d region");
    return 0;
}
