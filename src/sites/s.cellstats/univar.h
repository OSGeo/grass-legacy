#ifndef UNIVAR_H
#define UNIVAR_H 1
#include "cell_site.h"

/* univariate statistics structure */
struct ustruct
{
  int n;
  double m, s, cv, skw, skwb, kur, mse, mav, min, q1, med, q3, max;
};
typedef struct ustruct UNIV;

UNIV univariate (const Cell_Site *z, int nsites);
#endif
