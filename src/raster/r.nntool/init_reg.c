
#include "globals.h"

#define R Region

init_region()
{
int k;

  Region.area.define = 0;
  Region.area.completed = 0;
  Region.area.filled = 0;

  Region.npoints = 0;
  Region.view = NULL;

/*  for(k=0;k < MAX_POLYS;k++) { */
     Region.saved_npoints = 0;
     Region.saved_view = NULL;
     Region.area.saved = 0;
/*  } */

  Region.vertex_npoints = 0;
  Region.perimeter_npoints = 0;

}
