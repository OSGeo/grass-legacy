#include "globals.h"
#include "local_proto.h"

int 
erase_region (void)
{
  if (!Region.area.define)
    G_warning("Can not erase an undefined region.");
  else {
    while (del_point());
    return(0);
  }
  return 0;
}
