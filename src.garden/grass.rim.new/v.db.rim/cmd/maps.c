#define MAPS

#include <stdio.h>
#include "globals.h"
#include "rim.h"

maps()
{
  int error;
  char name[MAP_NAME_LENGTH+1], mapset[MAP_NAME_LENGTH+1];
  int idnum;

  /* check to see if a ref map with this number already exists */
  if (crim(MAP_TABLE, "select from referencemaps sort by map_id") == RIM_EOT) {
    fprintf(Outfile, "\nThere are no Reference Maps defined.\n");
    return(0);
  }

  fprintf(Outfile, "\n  Map Id#      Vector Map Name         Mapset Name \n");
  fprintf(Outfile, "=========================================================\n"
   );
  while ((error = crimdm(MAP_TABLE, GET, Rim_buffer)) != RIM_EOT) {
    if (error != 0) rim_error(error); /* any error other than EOT */
    ret_m_table(Rim_buffer, &idnum, name, mapset);
    fprintf(Outfile, "  %-13d %20s    %20s\n", idnum, name, mapset);
  }
}
