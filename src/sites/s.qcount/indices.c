/*
 * Copyright (C) 1994-1995. James Darrell McCauley. (darrell@mccauley-usa.com)
 * 	                          http://www.usol.com/~mccauley/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include "gis.h"
#include "s_struct.h"
#include "quaddefs.h"

void qindices (cnt, n, fisher, david, douglas, lloyd, lloydip, morisita)
  int *cnt, n;
  double *fisher, *david, *douglas, *lloyd, *lloydip, *morisita;

/*
 * Calculates quadrat count indices for measuring departure from complete
 * spatial randomness. See Cressie (1991).
 */
{
  int i;
  double m = 0, s = 0;

  *morisita = 0.0;
  for (i = 0; i < n; ++i)
  {
    m += cnt[i];
    *morisita += (double) cnt[i] * (cnt[i] - 1.0);
  }
  m /= n;

  for (i = 0; i < n; ++i)
    s += (cnt[i] - m) * (cnt[i] - m);
  s /= (n - 1);

  *fisher = s / m;
  *david = *fisher - 1;
  *douglas = m * m / (s - m);
  *lloyd = m + s / m - 1;
  *lloydip = *lloyd / m;
  *morisita *= n;
  *morisita /= (n*m * (n*m -1.0 ));  /* cressie */
  /* *morisita = *lloydip * n * m / (n * m - 1.0);  ripley 1 */
}
