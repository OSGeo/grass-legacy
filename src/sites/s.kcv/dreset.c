#include "gis.h"
#include "kcv.h"

void 
d_reset (D **d, int n)
{
  int i;

  for (i = 0; i < n; ++i)
    (*d)[i].i = -1;
  return;
}
