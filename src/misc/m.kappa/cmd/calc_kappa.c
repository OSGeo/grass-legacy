#include "mkappa.h"

calc_kappa()
{
  int i,j;
  double *pi, *pj, *pii, p0, pC;
  double kp, vkp, *kpp;
  double obs, inter1, inter2;
  long total;
  FILE *fd;

/* initialization */
  obs = 0;
  total = 0;
  inter1 = inter2 = 0;
  p0 = pC = 0;

  if (output == NULL)
    fd = stdout;
  else {
    if (!header)
      fd = fopen (output, "a");
    else
      fd = fopen (output, "w");
  }

  if (fd == NULL) {
    fprintf (stderr,
	"ERROR: can't open <%s> to write kappa and relevant parameters\n",
	output);
    return;
  }

/* calculate the parameters of the kappa-calculation */
  pi = (double *) G_calloc(ncat, sizeof(double));
  pj = (double *) G_calloc(ncat, sizeof(double));
  pii = (double *) G_calloc(ncat, sizeof(double));
  kpp = (double *) G_calloc(ncat, sizeof(double));

  for (i=0; i<ncat; ++i)
    for (j=0; j<ncat; ++j) {
      total += mat[i][j];
      pi[i] += mat[i][j];
      pj[j] += mat[i][j];
      pii[i] = mat[i][i];
    }

  for (i=0; i<ncat; ++i) {
    obs += pii[i];
    pi[i] /= total;
    pj[i] /= total;
    pii[i] /= total;
    p0 += pii[i];
    pC += pi[i] * pj[i];
  }

  for (i=0; i<ncat; ++i) 
    if ((pi[i] == 0) || (pj[i] == 0))
      kpp[i] = -999;
    else
      kpp[i] = (pii[i] - pi[i]*pj[i])/(pi[i] - pi[i]*pj[i]);

/* print out the commission and ommission error, and the conditional kappa */
  fprintf(fd, "Cats\t%% Commission\t%% Ommission\tEstimated Kappa\n");
  for (i=0; i<ncat; i++)
    if ((kpp[i] == -999) && (i != 0))
      fprintf(fd, "%ld\tNA\t\tNA\t\tNA\n", (long) i);
    else
      fprintf(fd, "%ld\t%lf\t%lf\t%lf\n",
	(long) i, 100*(1-pii[i]/pi[i]), 100*(1-pii[i]/pj[i]), kpp[i]);
  fprintf(fd, "\n");

/* calculate and print out kappa and total accuracy */
  for (i=0; i<ncat; ++i) {
    inter1 += pii[i]*pow(((1-pC)-(1-p0)*(pi[i]+pj[i])), 2.);
    for (j=0; j<ncat; ++j) {
      inter2 += mat[i][j]*pow((pi[i]+ pj[j]), 2.)/total;
    }
  }

  kp = (p0 - pC) /(1 - pC);
  vkp = (inter1 + pow((1-p0), 2.)*inter2 -  pow((p0*pC - 2*pC +p0), 2.)) / 
	pow((1-pC), 4.) / total;
  fprintf(fd, "Kappa\t\tKappa Variance\n");
  fprintf(fd, "%lf\t%lf\n", kp, vkp);
  fprintf(fd, "\n");

  fprintf(fd, "Obs Correct\tTotal Obs\t%% Observed Correct\n");
  fprintf(fd, "%ld\t\t%ld\t\t%lf\n", (long) obs, total, 100*obs/total);

  if (output != NULL)
    fclose(fd);
  free(pi);
  free(pj);
  free(pii);
  free(kpp);
}
