#include "Vect.h"


/*
   ** Simply returns level that Map is opened at
   **  returns open level or -1 on error
 */
int
Vect_level (Map)
     struct Map_info *Map;
{
  if (Map->open != VECT_OPEN_CODE)
    {
      if (Map->open != VECT_CLOSED_CODE)
	fprintf (stderr, "VECT_LEVEL: Map structure was never initialized\n");
      else
	fprintf (stderr, "VECT_LEVEL: Map structure has been closed\n");
      return (-1);
    }
  return (Map->level);
}
