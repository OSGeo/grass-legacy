#ifndef lint
static char *SCCSID = "@(#)mproj.c	AMG v.3.1";
#endif
/* projection to scaled map - double output */

# include "mapgen.h"

extern struct map_def def;

mproj(lon, lat, x, y) double lon, lat, *x, *y; {
	double px, py;
	int ret;

	if (!(ret = approj(lon, lat, &px, &py))) {
		*x = def.cosr * px - def.sinr * py + def.xf_off;
		*y = def.sinr * px + def.cosr * py + def.yf_off;
	}
	return (ret);
}
