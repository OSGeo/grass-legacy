#include "Vect.h"


int
Vect_read_next_line (Map, line_p, line_c)
     struct Map_info *Map;
     struct line_pnts *line_p;
     struct line_cats *line_c;
{
  if (!VECT_OPEN (Map))
    return -1;

  switch (Map->level)
    {
    case 1:
      return V1_read_next_line (Map, line_p, line_c);
      /*NOTREACHED */
      break;
    case 2:
      return V2_read_next_line (Map, line_p, line_c);
      /*NOTREACHED */
      break;
    case 3:
    default:
      return -1;
    }

  /*NOTREACHED */
}
