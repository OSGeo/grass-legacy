#ifndef _GRASS_CC_H
#define _GRASS_CC_H

/* datum.c */
int CC_datum_shift(const char *, double *, double *, double *);
char *CC_datum_name(int);
char *CC_datum_description(int);
/* format_ll.c */
int CC_lat_format(double, char *);
int CC_lon_format(double, char *);
int CC_lat_parts(double, int *, int *, double *, char *);
int CC_lon_parts(double, int *, int *, double *, char *);
/* geocen1.c */
int CC_ll2geo(double, double, double, double, double, double *, double *, double *);
/* geocen2.c */
int CC_geo2ll(double, double, double, double, double, double *, double *, double *, int, double);
/* scan_ll.c */
int CC_lat_scan(char *, double *);
int CC_lon_scan(char *, double *);
/* spheroid.c */
int CC_get_spheroid(const char *, double *, double *);
char *CC_spheroid_name(int);
/* tm.c */
int CC_tm2ll_spheroid(char *);
int CC_tm2ll_spheroid_parameters(double, double);
int CC_tm2ll_zone(int);
int CC_tm2ll_north(double);
int CC_tm2ll(double, double *, double *);
int CC_ll2tm(double, double, double *, double *, int *);
/* utm.c */
int CC_u2ll_spheroid(char *);
int CC_u2ll_spheroid_parameters(double, double);
int CC_u2ll_zone(int);
int CC_u2ll_north(double);
int CC_u2ll(double, double *, double *);
int CC_ll2u(double, double, double *, double *, int *);

#endif
