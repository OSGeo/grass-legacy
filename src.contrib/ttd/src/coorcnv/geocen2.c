/* %W% %G% */
/***************************************************************
* CC_geo2ll (a, e, x, y, z, lat, lon, h, n, stop)
*
* convert geocentric coordinates (x,y,z) to geographic (lat,lon,h)
*
* inputs
*  a     major axis of local spheroid
*  e     square of eccentricity of spheroid
*  x,y,z geocentric coordinates (in meters)
*  n     maximum number of iterations when solving equations
*  stop  stop delta
*
* outputs
*  lat   latitude in arc seconds
*  lon   longitude in arc seconds
*  h     height in meters above spheroid
*
*  returns: number iterations remaining, or 0 if did not converge
*****************************************************************/

#define RADIANS_TO_SECONDS 206264.8062470964 

CC_geo2ll (a, e, x, y, z, lat, lon, h, n, stop)
    double a, e, x, y, z;
    double *lat, *lon, *h;
    int n;
    double stop;
{
    double T, t;
    double phi;
    double R;
    double ae;
    double N;
    double zpt;
    double sinphi;
    double asin(), sin(), sqrt(), atan(), atan2();
    double dabs();
    int i;

    if (n <= 0) n = 1 ;
    stop = dabs(stop) ;

    R = x*x + y*y ;
    T = z / (1 - e) - z ;
    ae = a * e;

    for (i = 1; i <= n; i++)
    {
	zpt = z + T ;
	phi = asin ( zpt / sqrt(R + zpt*zpt) ) ;

	sinphi = sin(phi);
	t = ae*sinphi / sqrt (1 - e*sinphi*sinphi);

	if (dabs(t-T) <= stop) break;
	T=t;
    }

    *lat = phi * RADIANS_TO_SECONDS ;
    *lon = atan2 (y,x) * RADIANS_TO_SECONDS ;

    zpt = z + t;
    *h = sqrt (R + zpt*zpt) - a / sqrt (1 - e*sinphi*sinphi);

/* return number of iterations remaining */
/* ie, if max iterations were used return 0 */
    return i > n ? 0 : i;
}

static double
dabs(x)
    double x;
{
    return x < 0 ? -x : x;
}
