#include "gis.h"

char *
openvect (char *name)
{
  char *mapset;

  mapset = G_find_vector2 (name, "");

  if (mapset == NULL)
    fprintf (stderr, "warning: %s - vector file not found\n", name);
  return mapset;
}
