static char *SCCSID = "@(#)enfn_.c	AMG v.1.4";
/* meridinal distance for ellipsoid */
# include <math.h>
# define C02	.25
# define C04	.046875
# define C06	.01953125
# define C22	.375
# define C24	.09375
# define C26	.0439453125
# define C44	.05859375
# define C46	.0439453125
# define C66	.01139322916666666666
	void
enfn_(en, es) double *en, es; {
	*en++ = 1. - es * (C02 + es * (C04 + es * C06));
	*en++ = es * (C22 + es * (C24 + es * C26));
	*en++ = es * es * (C44 + es * C46);
	*en = es * es * es * C66;
}
	double
mlfn_(en, phi) double en[], phi; {
	double sum = phi, t;
	sum = sum * en[0] - en[1] * sin(phi += phi);
	sum += en[2] * sin(t = phi + phi);
	return ( sum - en[3] * sin(t + phi) );
}
