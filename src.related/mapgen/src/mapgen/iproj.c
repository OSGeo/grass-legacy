#ifndef lint
static char *SCCSID = "@(#)iproj.c	AMG v.3.1";
#endif
/* projection to scaled map - long input and output */

# include "mapgen.h"

extern struct map_def def;

iproj(lon, lat, x, y) long lon, lat, *x, *y; {
	double px, py;

	approj(ID_CON * lon, ID_CON * lat, &px, &py);
	*x = def.cosr * px - def.sinr * py + def.xf_off;
	*y = def.sinr * px + def.cosr * py + def.yf_off;
}
