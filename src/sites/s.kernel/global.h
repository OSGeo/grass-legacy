#include "gis.h"
#include "site.h"

typedef struct
/*management of lists of site files*/
{
  char *name;   /*name of site file*/
  Site **sites; /*sites contained within the site file*/
  int nsites;   /*number of sites*/
} ListSite;

int read_list_of_sites();
void compute_distance();
double euclidean_distance();
double gaussian2dBySigma();
double invGaussian2d();
double gaussian2dByTerms();
int readSitesFiles();
double kernel1();
double segno();
