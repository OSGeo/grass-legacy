#ifndef lint
static char *SCCSID = "@(#)rtodms.c	USGS v.3.2";
#endif
/* Convert radian argument to DMS format (resolution to .001") */
# include <math.h>
/*
** RES is fractional second figures
** RES60 = 60 * RES
** CONV = 180 * 3600 * RES / PI (radians to RES seconds)
*/
#define PI	3.14159265358979323844
	static double
RES = 1000.,
RES60 = 60000.,
CONV = 206264806.24709635515796003417;
	static char
format[50] = "%dd%d'%.3f\"%c";
	static int
dolong = 0;
	void
set_rtodms(fract, con_w) int fract; { /* fract == # fractional digits */
	int i;

	if (fract >= 0 && fract < 9 ) {
		RES = 1.;
		for (i = 0; i < fract; ++i)
			RES *= 10.;
		RES60 = RES * 60.;
		CONV = 180. * 3600. * RES / PI;
		if (! con_w)
			sprintf(format,"%%dd%%d'%%.%df\"%%c", fract);
		else
			sprintf(format,"%%dd%%02d'%%0%d.%df\"%%c",
				fract+2+(fract?1:0), fract);
		dolong = con_w;
	}
}
	char *
rtodms(s, r, pos, neg) char *s; double r; {
	int deg, min, sign;
	char *ss = s;
	double sec;

	if (r < 0) {
		r = -r;
		if  (!pos) { *ss++ = '-'; sign = 0; }
		else sign = neg;
	} else
		sign = pos;
	r = floor(r * CONV + .5);
	sec = fmod(r / RES, 60.);
	r = floor(r / RES60);
	min = fmod(r, 60.);
	deg = r / 60.;
	if (dolong)
		sprintf(ss,format,deg,min,sec,sign);
	else if (sec) {
		char *p, *q;

		sprintf(ss,format,deg,min,sec,sign);
		for (q = p = ss + strlen(ss) - (sign?3:2); *p == '0'; --p) ;
		if (*p != '.')
			++p;
		if (++q != p)
			strcpy(p, q);
	} else if (min)
		sprintf(ss,"%dd%d'%c",deg,min,sign);
	else
		sprintf(ss,"%dd%c",deg, sign);
	return s;
}
