#include "Vect.h"

/*
   **  Writes line info to current position (should be EOF) in output map
   **
   **  returns offset in file that (start of) line is written at.
 */

long
Vect_write_line (Map, type, points, cats)
     struct Map_info *Map;
     int type;
     struct line_pnts *points;
     struct line_cats *cats;
{
  return Vect__Write_line (Map, type, points, cats);
}
