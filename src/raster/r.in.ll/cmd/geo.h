#include "gis.h"
struct GEO
{
    double lon;
    double lat;
    double lat_res;
    double lon_res;
    int ncols;
    int nrows;
    int bpc;
    int sflag;
    double a,e;	/* spheroid parameters */
};

#ifdef GRASS_ROWIO_H
/* geo_value.c */
int geo_value(double, double, struct GEO *, ROWIO *);
#endif

/* getargs.c */
int getargs(int, char *[], struct GEO *, char **, char **);
/* row_col.c */
int row_col(struct GEO *, double, double, int *, int *);
