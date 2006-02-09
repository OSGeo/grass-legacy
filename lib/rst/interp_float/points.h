
/*
**  Written by H. Mitasova, I. Kosinovsky, D. Gerdes  Spring 1992
**  US Army Construction Engineering Research Lab
**  Copyright  H. Mitasova, I. Kosinovsky, D.Gerdes  USA-CERL  1992
*/
#include <grass/gis.h>

/* for resample program */
struct fcell_triple   {
  double x;
  double y;
  FCELL z;
  double smooth;
};


