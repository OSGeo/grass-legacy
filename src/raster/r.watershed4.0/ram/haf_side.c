#include "Gwater.h"

haf_basin_side (updir,downdir,thisdir)
SHORT updir, downdir, thisdir;
{
	SHORT newup, newthis;

      newup = updir - downdir;
      if (newup < 0) newup += 8;
      newthis = thisdir - downdir;
      if (newthis < 0) newthis += 8;
      if (newthis < newup) return (LEFT);
      if (newthis > newup) return (RITE);
      return (NEITHER);
}
