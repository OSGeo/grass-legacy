#ifdef FIPS_BSD

#include "fips_bsd.h"

ldiv_t
ldiv ( x, y )
     long x, y;
{
  ldiv_t d;

  d.quot = x/y;
  d.rem = x%y;

  if (d.rem < 0)
    {
      d.quot --;
      d.rem += y;
    }

  return (d);
}

#endif
