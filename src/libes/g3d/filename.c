#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/

void
G3d_filename (path, elementName, mapName, mapset)

     char *path, *elementName, *mapName, *mapset;

{
  char map[300];

  sprintf(map, "%s/%s", G3D_DIRECTORY, mapName);
  G__file_name (path, map, elementName, mapset);
}
