#include <math.h>
#include "gis.h"

double *solvex (a, n, m, bee)
  int n, m;
  double **a, *bee;

/*
 * Given the design matrix {\tt a} ($\mbox{\tt m} \times \mbox{\tt n}$) and
 * the vector {\tt bee}, return $x\/$ such that $$ \mbox{\tt A}x \approx
 * \mbox{\tt bee}.$$
 */
{
  int i, j;
  double *singular_value, **v, max, *x;

  /* sanity check */
  if (m < n)
    G_fatal_error ("Need m >= n to obtain least squares fit");

  x = G_alloc_vector (n);
  singular_value = G_alloc_vector (m);
  v = G_alloc_matrix (m,m);
  if (v==NULL || v[0]==NULL || x==NULL || singular_value == NULL)
    G_fatal_error("Memory error in solvex()");
  G_svdcmp(a,m,n,singular_value,v);
  for(i=0, max=0.0;i<n;++i)
     if (singular_value[i] > max)
       max=singular_value[i];
  for(i=0;i<n;++i)
     if (singular_value[i] < 1.0e-6*max)
       singular_value[i]=0.0;
  G_svbksb(a,singular_value,v,m,n,bee,x);

  return x;
}
