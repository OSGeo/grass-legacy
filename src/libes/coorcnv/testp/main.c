#include "CC.h"
#include "gis.h"


int main (int argc, char *argv[]) {
  struct pj_info *myprojkeys;
  struct Key_Value *myprojname;
  char *ellps_name;
  double a, e2, f, dx, dy, dz;

  G_gisinit(argv[0]);

  myprojkeys = G_get_projinfo();
  if ( myprojkeys != NULL ) {
    myprojname = G_find_key_value("proj", myprojkeys);
    printf("Projection from PROJ_INFO: %s\n", myprojname);
  }

  printf("Projection of this location: %s\n",G_database_projection_name());
  ellps_name = G_database_ellipse_name();
  printf("Ellipsoid name of this location: %s\n",ellps_name);
  G_get_ellipsoid_parameters(&a, &e2);
  printf("Ellipsoid parameters of this location: a=%.8f e2=%.8f\n",a, e2);
  printf("Map datum of this location: %s\n",G_database_datum_name());
  G_get_datum_parameters(&a, &e2, &f, &dx, &dy,& dz);
  printf("Datum parameters: dx=%.2f dy=%.2f dz=%.2f\n", dx, dy, dz);
  printf("Ellipsoid parameters: a=%.8f e2=%.8f 1/f=%.6f\n", a, e2, f);
  printf("\n\n");

  exit(0);
}
