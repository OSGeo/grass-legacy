#include "globals.h"

remove_mask()
{
  static char *elements[] = {"cell", "cellhd", "cats", "colr", 
		     "colr2", "hist", "cell_misc", ""};
  int i = 0;

  while (strcmp(elements[i], "") != 0)
    if (G_remove(elements[i++], "MASK") <0)
      G_fatal_error("Error while removing the old MASK cell map.");

}
