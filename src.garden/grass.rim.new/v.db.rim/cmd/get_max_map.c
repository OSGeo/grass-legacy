#include <stdio.h>
#include "globals.h"
#include "rim.h"

int get_max_map()
{
  int error;
  char name[MAP_NAME_LENGTH+1], mapset[MAP_NAME_LENGTH+1];
  int idnum;

  /* check to see if a ref map exists */
  if (crim(MAP_TABLE, "select from referencemaps sort by map_id") == RIM_EOT) {
    return(0);
  }

  /* step out to the last record */
  while ((error = crimdm(MAP_TABLE, GET, Rim_buffer)) != RIM_EOT) {
    if (error != 0) rim_error(error); /* any error other than EOT */
  }

  /* get the id_num from the last one and return it */
  ret_m_table(Rim_buffer, &idnum, name, mapset);
  return(idnum);
}
