#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "CC.h"
#include "globals.h"

int get_point_coords (char *p, double *x, double *y)
{
ll_from_str(p,x,y);
if (proj==PROJECTION_UTM) make_utms(x,y,&zone,1);
    return 0;
}

int 
ll_from_str (char *cp, double *lon, double *lat)
{
	char s[20];

	/* avoid sscanf for speed */
	strncpy(s,cp,4);     /* get int part  of lon */
	*(s+4) = '.' ;       /* add dec. pt. */
        strncpy(s+5,cp+4,6); /* get 6 dec's  */
        *(s+10) = '\0';      /* add null */
	*lon = atof(s); /* convert it */

	strncpy(s,cp+10,3);  /* get int part  of lat */
	*(s+3) = '.' ;       /* add dec. pt. */
        strncpy(s+4,cp+13,6);/* get 6 dec's  */
        *(s+10) = '\0';      /* add null */
	*lat = atof(s); /* convert it */
	return 0;
}

static int init_once=0;

int 
make_utms (double *x, double *y, int *zone, int n)
{
int i;
double lt, ln;

	if (!init_once) {
		if (proj!=PROJECTION_UTM)
		  G_fatal_error("Can't do UTM conversions in this location!");
		if ((CC_u2ll_spheroid(sphere)!=1))
			G_fatal_error(
		"\nBad spheroid.  See m.gc.ll manual page for choices.");
		init_once = 1;
	}
	for (i=0; i<n; i++) {
		ln = x[i]*3600.0;
		if (ln == 0.0) return 0; /* done if x=0 */
		if (ln < 0.0) ln = -ln;
		lt =  y[i]*3600.0;
		CC_ll2u( lt, ln, &x[i], &y[i], zone);
	}
	return 0;
}

