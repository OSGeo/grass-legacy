#include<math.h>
#include "gis.h"
#include "idsfft.h"

#define max(a,b) ((a) >= (b) ? (a) : (b))

void do_idsfft (z, ndp, file, verbose, xi, yi, nip)
  int ndp, nip, verbose;
  char *file;
  float *xi, *yi;		/* coord. interpolation pnts */
  Z *z;
{
  extern struct Cell_head window;
  int i, j;
  int nxi;			/* no. interpolation pnts e-w dir */
  int nyi;			/* no. interpolation pnts n-s dir */
  float *zi;			/* interpolated values */
  float *xd, *yd, *zd;		/* data pnts */
  extern int idbvip_ (), idsfft_ ();
  static int ncp = 4;		/* no. xtra pnts 4 part. deriv. */
  static int md = 1;		/* computation mode */
  float *wk;			/* internal work area */
  int *iwk;			/* internal work area */

  xd = (float *) G_malloc (ndp * sizeof (float));
  yd = (float *) G_malloc (ndp * sizeof (float));
  zd = (float *) G_malloc (ndp * sizeof (float));
  if (xd == NULL || yd == NULL || zd == NULL)
    G_fatal_error ("Memory allocation for data points");
  for (i = 0; i < ndp; ++i)
  {
    xd[i] = z[i].x;
    yd[i] = z[i].y;
    zd[i] = z[i].z;
  }

  /* define output grid (size and coordinates) */
  if (nip <= 0)
  {
    nyi = (int) ((window.north - window.south) / window.ns_res) + 1;
    nxi = (int) ((window.east - window.west) / window.ew_res) + 1;
    xi = (float *) G_malloc (nxi * sizeof (float));
    yi = (float *) G_malloc (nyi * sizeof (float));
    zi = (float *) G_malloc (nxi * nyi * sizeof (float));
    if (xi == NULL || yi == NULL || zi == NULL)
      G_fatal_error ("Memory allocation for i data");
    for (i = 0; i < nxi; ++i)
      xi[i] = window.west + i * window.ew_res;
    for (i = 0; i < nyi; ++i)
      yi[i] = window.south + i * window.ns_res;
    i = max (31, 27 + ncp) * ndp + nxi * nyi;
    j = 8;
  }
  else
  {
    i = max (31, 27 + ncp) * ndp + nip;
    j = 5;
    zi = (float *) G_malloc (nip * sizeof (float));
    if (zi == NULL)
      G_fatal_error ("Memory allocation for zi data");
  }

  /* allocate work space */
  wk = (float *) G_malloc (j * ndp * sizeof (float));
  iwk = (int *) G_malloc (i * sizeof (int));
  if (wk == NULL || iwk == NULL)
    G_fatal_error ("Memory allocation for work space");

  /* do interpolation (calculate zi) */
  if (nip <= 0)
  {
    if (verbose)
      fprintf (stderr, "Interpolating ...                   ");
    idsfft_ (&md, &ncp, &ndp, xd, yd, zd, &nxi, &nyi, xi, yi, zi, iwk, wk, verbose);
  }
  else
  {
    if (verbose)
      fprintf (stderr, "Cross Validating ...                ");
    j = md = 1;
    for (i = 0; i < nip; ++i)
    {
      if (i != 0) md = 2; 
      /*-idbvip_(&md, &ncp, &ndp, xd, yd, zd, &c__1, &xi[ixi - 1], &yi[iyi 
                    - 1], &zi1[ixi + iyi * 6 - 7], iwk, wk); */
      if (verbose)
        G_clicker();
      idbvip_ (&md, &ncp, &ndp, xd, yd, zd, &j, 
               &xi[i], &yi[i], &zi[i], iwk, wk);
    }
  }

  if (verbose)
    G_percent (1, 1, 1);

  /* write output */
  if (file != NULL)
  {
    if (nip <= 0)
      writesites2 (file, xi, yi, zi, nxi, nyi, verbose); /* interpolation */
    else
      writesites (file, xi, yi, zi, nip, verbose); /* cross validation */
  }
  else
  {
    if (nip <= 0) /* interpolation */
      for (j = 0; j < nxi; ++j)
      {
	for (i = 0; i < nyi; ++i)
	  fprintf (stdout, "%g %g %g\n", xi[j], yi[i], zi[j + i * (nyi + 1)]);
        fprintf (stdout, "\n");
      }
    else /* cross validation */
      for (i = 0; i < nip; ++i)
	fprintf (stdout, "%g %g %g\n", xi[i], yi[i], zi[i]);
  }
  free (xd);
  free (yd);
  free (zd);
  if (nip <= 0)
  {
    free (xi);
    free (yi);
  }
  free (zi);
  free (wk);
  free (iwk);
}
