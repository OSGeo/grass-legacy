#include <stdio.h>
#include "glob.h"
#include "G3d.h"
#include "local_proto.h"
void
set_window (window, name)
    G3D_Region *window;
    char * name;
{
    if(! G3d_writeWindow (window, G3D_DEFAULT_WINDOW))
	fprintf (stderr, "** unable to write current region. ");
    else {
      G3d_setWindow (window);
      fprintf (stderr, "current region set from [%s]. ", name);
    }
}
