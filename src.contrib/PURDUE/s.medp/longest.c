#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "polish.h"

double longestline (Map)
struct Map_info *Map;
{
  int i;
  int cur_line;
  int ab_line;
  double *xptr1, *yptr1;
  double *xptr2, *yptr2;
  double len, max_len;
  static struct line_pnts Points;

  for(max_len=0,cur_line = 1; cur_line < Map->n_lines ; cur_line++)
  {
    V2_read_line (Map, &Points, cur_line);

    xptr1 = Points.x;
    yptr1 = Points.y;
    xptr2 = Points.x + 1;
    yptr2 = Points.y + 1;

    if (Points.n_points > 2)
      G_warning("Line with more than 2 nodes");

    for(i=1; i< Points.n_points; i++)
    {
      len = hypot(*xptr1-*xptr2, *yptr1-*yptr2);
      max_len = (len > max_len) ? len : max_len;
      xptr1++ ; xptr2++ ; yptr1++; yptr2++;
    }
  }
  return max_len;
}

