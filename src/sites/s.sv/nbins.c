#include<math.h>
#include"gis.h"

int 
nbins (double nominal_lag)
{
  struct Cell_head window;

  G_get_window (&window);
  return 1 + (int) ceil (hypot(window.east-window.west,
                               window.north-window.south)
                         /nominal_lag);
}
