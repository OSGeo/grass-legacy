
#include "globals.h"

erase_region()
{
  if (!Region.area.define)
    G_warning("Can not erase an undefined region.");
  else {
    while (del_point());
    return(0);
  }
}
