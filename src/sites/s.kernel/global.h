#include "gis.h"
#include "site.h"
#define PIG           3.14159265358979323846 

typedef struct
/*manegement of lists of site files*/
{
  char *name; /*name of site file*/
  Site **sites; /*sites contained within the site file*/
  int nsites; /*number of sites*/
} ListSite;

int read_list_of_sites();
void compute_distance();
double euclidean_distance();
