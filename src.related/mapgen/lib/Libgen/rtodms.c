#ifndef lint
static char *SCCSID = "@(#)rtodms.c	AMG v.3.1";
#endif
/*	Convert radian argument to DMS format (resolution to .001") */
# include <math.h>

# define CONV 206264806.24709635515796003417

char *
rtodms(s, r, pos, neg)
char *s;
double r;
{
	int deg, min, sign;
	double sec;

	if (r < 0) {
		sign = neg;
		r = -r;
	} else
		sign = pos;
	r = floor(r * CONV + .5);
	sec = fmod(r / 1000., 60.);
	r = floor(r / 60000.);
	min = fmod(r, 60.);
	deg = r / 60.;
	if (sec) {
		char *p, *q;

		sprintf(s,"%dd%d'%.3f\"%c",deg,min,sec,sign);
		for (q = p = s + strlen(s) - 3; *p == '0'; --p) ;
		if (*p != '.')
			++p;
		if (++q != p)
			strcpy(p, q);
	} else if (min)
		sprintf(s,"%dd%d'%c",deg,min,sign);
	else
		sprintf(s,"%dd%c",deg, sign);

	return s;
}
