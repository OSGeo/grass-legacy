/* some tests collected by
 * andreas.lange@rhein-main.de
 *
 */

#include "gis.h"
#include "CC.h"

#define GRASSNEW 
#define DATUM
#define ELLIP
/* #define CCTRS */

const char rcsid[] = "@(#)$Header$";

int 
main (int argc, char *argv[])
{

  int i = 0, n = 0;
  char *datum_name = NULL, *datum_descr = NULL, 
    *ellps_name = NULL, *prj = NULL;
  char ellps[100];
  double a, e2, f, dx, dy, dz;

  G_gisinit(argv[0]);

  /* if (G_parser(argc, argv) < 0)
    exit(-1); */

  /* print out all known projections */
  printf("\n\nProjections:\n");
  printf("nbr \t name\n\n");
  
  for (i=0; ; i++) {
    prj = G__projection_name(i);
    if (prj == NULL)
      break;
    printf("%d \t %s\n",i+1,prj);
  }

  /* print out all known ellipsoids */
#ifdef ELLIP
  printf("\n\nEllipsoids:\n");
  printf("nbr \t name \t description \t data \n"); 
  for (i = 0; ; i++) 
    {
      n = i;
      ellps_name = CC_spheroid_name(i);
      if (ellps_name == NULL)
	break;
      printf("%d \t %s \t %s ", i+1, ellps_name, G_ellipsoid_description(i));
      if (CC_get_spheroid (ellps_name, &a, &e2) == 1)
	printf("\t a=%.2lf e2=%.6lf\n", a, e2);
      if ( (n = CC_get_spheroid_by_name(ellps_name, &a, &e2, &f)) != -1) 
	printf("%d \t %s \t \t \t a=%.2lf e2=%.6lf f=1/%3.4lf\n", n+1, G_ellipsoid_name(i), a, e2, f);  
      if ( (ellps_name = CC_get_spheroid_by_nbr(i)) != NULL )
	printf("%d \t %s\n", i+1, ellps_name);
    }
#endif


  /* print out all known map datums */
#ifdef DATUM
  printf("\n\nMap Datums:\n");
  printf("nbr \t name \t description \t data \n");
  for (i = 0; ;i++) 
    {
      if ((datum_name = CC_datum_name(i)) == NULL)
	break;
      printf("%d \t %s \t", i, datum_name);
      datum_descr = CC_datum_description(i);
      if (datum_descr != NULL)
	printf("\t %s ",datum_descr); 
      if (CC_datum_shift(datum_name, &dx, &dy, &dz) == 1)
	printf("\t dx=%3.3lf dy=%3.3lf dz=%3.3lf\n", dx, dy, dz);
      if (CC_get_datum_parameters(datum_name, ellps, &dx, &dy, &dz) == 1)
	printf("\t \t ellips: %s \t dx=%3.3lf dy=%3.3lf dz=%3.3lf\n", ellps, dx, dy, dz);
      if ( (ellps_name = CC_datum_ellipsoid(i)) != NULL)
	printf("\t \t ellips: %s", ellps_name);
      G_get_spheroid_by_name(ellps_name, &a, &e2, &f);
      printf("\t a=%.2lf e2=%.6lf f=1/%3.4f \n", a, e2, f);    
      datum_name = CC_get_datum_by_nbr(i);
      printf("%d \t %s \n", i, datum_name);
    }
#endif

  /* print out projection information */
  printf("\n\nProjection of this location: %s\n",G_database_projection_name());
#ifdef GRASSNEW
  ellps_name = G_database_ellipse_name();
  printf("Ellipsoid name of this location: %s\n",ellps_name);
#endif GRASSNEW
  G_get_ellipsoid_parameters(&a, &e2);
  printf("Ellipsoid parameters of this  location: a=%.8f e2=%.8f\n",a, e2);

#ifdef GRASSNEW
  printf("\n\nMap datum of this location: %s\n",G_database_datum_name());
  G_get_datum_parameters(&a, &e2, &f, &dx, &dy,& dz);
  printf("Datum parameters: dx=%.2f dy=%.2f dz=%.2f\n", dx, dy, dz);
  printf("Ellipsoid parameters: a=%.8f e2=%.8f 1/f=%.6f\n", a, e2, f);
#endif GRASSNEW
  printf("\n\n");


#ifdef CCTRS

  /* some hardcoded tests: */
  /* example from Peter H. Dana: */
  double Sphi = 30.0;
  double Slam = -100.0;
  double Sh   = 0; /* 232.0; */
  double Sa   = 6378206.4;
  double rSf  = 294.9786928;
  double Da   = 6378137.0;
  double rDf  = 298.257223563;
  double dx   = -8.0;
  double dy   = 160.0;
  double dz   = 176.0;
  double Dphi = 0;
  double Dlam = 0;
  double Dh   = 0;
  double Se, De, Sf, Df;

  Sf = (double)1.0/rSf;
  Df = (double)1.0/rDf;
  Se = 2.0 * Sf - Sf * Sf;
  De = 2.0 * Df - Df * Df;

  /*
  printf("lat %lf lon %lf h %f\n", Sphi, Slam, Sh);
  / Sphi *= 3600;
     Slam *= - 3600; /
  if (CC_lld2geo(Sa, Se, Sphi, Slam, Sh, &x, &y, &z) == 1)
    printf("x %lf y %lf z %lf\n", x, y, z);
  CC_geo2lld(Sa, Se, x, y, z, &Sphi, &Slam, &Sh);
  /   CC_geo2ll(Sa, Se, x, y, z, &Sphi, &Slam, &Sh, 50, 1.0e-6);
       Sphi /= 3600;
     Slam /= - 3600; /
     printf("lat %lf lon %lf h %f\n", Sphi, Slam, Sh); */

  printf("\n\nlat %lf lon %lf h %f\n", Sphi, Slam, Sh);
 
  /*  printf("CC block: \n");

  if (CC_datum_shift_CC(Sphi, Slam, Sh, Sa, Se, &Dphi, &Dlam, &Dh, Da, De, dx, dy, dz) == 0)
    printf("ERROR\n");

  printf("lat: %lf lon: %lf h: %f \n", Dphi, Dlam, Dh);

  printf("Molodensky: \n");
  if (CC_datum_shift_Molodensky(Sphi, Slam, Sh, Sa, Se, rSf, &Dphi, &Dlam, &Dh, Da, De, rDf, dx, dy, dz) == 0)
    printf("ERROR\n");

  printf("lat: %lf lon: %lf h: %f \n", Dphi, Dlam, Dh);
  
  printf("Bursa Wolf: \n");

  if (CC_datum_shift_BursaWolf(Sphi, Slam, Sh, Sa, Se, &Dphi, &Dlam, &Dh, Da, De, dx, dy, dz, 0, 0, 0, 0) == 0)
    printf ("ERROR\n");

*/

  if (CC_datum_to_datum_shift_CC(1,Sphi,Slam,Sh,4,&Dphi,&Dlam,&Dh) == 0)
    printf("ERROR\n");
  
  printf("lat: %lf lon: %lf h: %f \n", Dphi, Dlam, Dh); 

#endif

  exit (0);
}
