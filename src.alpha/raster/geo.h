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
    int lflag;
    int sflag;
    double a,e;	/* spheroid parameters */
};
