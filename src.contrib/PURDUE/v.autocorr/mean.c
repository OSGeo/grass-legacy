#include "gis.h"
#include "ranlib.h"

#define SAMPLE 500
/* Usage: mean(catarray,&mgrr,&mmcr,&mgrn,&mmcn,n) */

mean (catarray, mgrr, mmcr, mgrn, mmcn, n)
  double catarray[], *mgrr, *mmcr, *mgrn, *mmcn;
  int n;
{
  int i, j, k, sample = 0;
  long *tmparray;
  double *tmpcat, tmpgr, tmpmc, savegr[SAMPLE], savemc[SAMPLE];
  float catav, catsd;		/* average and std dev */
  float average ();
  extern struct Categories cats;
  double att2cat ();

  tmparray = (long *) G_malloc ((n) * sizeof (long));
  if (tmparray == NULL)
    G_fatal_error ("Memory allocation error for temporary array (mean)");
  tmpcat = (double *) G_malloc ((n + 1) * sizeof (double));
  if (tmpcat == NULL)
    G_fatal_error ("Memory allocation error for temporary array (mean)");

  for (j = 0; j < n; ++j)
    tmparray[j] = (long) j + 1;

  i = 0;
  while (i < SAMPLE)
  {
    genprm (tmparray, n);
    for (j = 1; j <= n; ++j)
      tmpcat[j] = catarray[(int) tmparray[j - 1]];
    geary_moran (tmpcat, &tmpgr, &tmpmc, n);
    savegr[i] = tmpgr;
    savemc[i++] = tmpmc;
  }
  *mgrr = (double) average (savegr, SAMPLE);
  *mmcr = (double) average (savemc, SAMPLE);

  /*
   * Actually should declare savegr and savemc as pointers, malloc 100 or so
   * doubles, calculate for 100 different permutations. If (!WS_test()),
   * malloc 100 more, calculate WS_test() each time until either we're okay
   * or we reach 200 (printing a warning in the latter case)
   */

  /* Calculate mean and stderr of catarray, store in catmean, catstd */
  stats (catarray, n, &catav, &catsd);
  i = 0;
  while (i < SAMPLE)
  {
    for (j = 1; j <= n; ++j)	/* Generate normal deviate */
      tmpcat[j] = (double) gennor (catav, catsd);
    geary_moran (tmpcat, &tmpgr, &tmpmc, n);
    savegr[i] = tmpgr;
    savemc[i++] = tmpmc;
  }
  *mgrn = (double) average (savegr, SAMPLE);
  *mmcn = (double) average (savemc, SAMPLE);
}

float average (array, n)
  double array[];
  int n;
{
  int i;
  float mean = 0;

  for (i = 0; i < n; ++i)
    mean += array[i];
  return (mean / n);
}

stats (array, n, catmean, catstd)
  double array[], *catmean, *catstd;
  int n;
{
  int i;

  for (i = 1; i <= n; ++i)
    *catmean += array[i];
  *catmean /= n;

  for (i = 1; i <= n; ++i)
    *catstd += (array[i] - *catmean) * (array[i] - *catmean);
  *catstd *= 1.0 / (n - 1);
  *catstd = sqrt (*catstd);
}
