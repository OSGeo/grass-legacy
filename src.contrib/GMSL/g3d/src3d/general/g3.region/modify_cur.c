#include "gis.h"
#include "glob.h"
#include "G3d.h"
#include "local_proto.h"
int
modify_cur()
{
  G3D_Region window;

  G3d_getWindow (&window);
  if (edit_window(&window)) {
    G3d_setWindow (&window);
    G3d_writeWindow (&window, G3D_DEFAULT_WINDOW);
  }

  return 1;
}
