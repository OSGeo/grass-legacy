#ifndef lint
static char *SCCSID = "@(#)set_line.c	OEMG v.1.1";
#endif
/* interprets standard line mode control string */
# include <graphics.h>
# include "gen.h"
extern long strtol();
extern double strtod();
	static long
mask = -1,
dsize = 0,
fdist = 0,
fsize = 0;
	void
set_line(s) char *s; {
	if (*s && *s != ',')
		plotopt(MPEN, strtol(s, &s, 0));
	if (*s && *++s && *s != ',') {
		mask = strtol(s, &s, 0);
		if (mask <= 0)
			plotopt(SOLID);
		else {
			plotopt(DMASK, mask);
			plotopt(DASH);
		}
	}
	if (*s && *++s && *s != ',')
		plotopt(DSIZE, dsize = (long)(strtod(s, &s) * cts_cm));
	if (*s && *++s && *s != ',')
		plotopt(F_DIST, fdist = (long)(strtod(s, &s) * cts_cm + .5));
	if (*s && *++s && *s != ',')
		plotopt(F_SIZE, fsize = fcharsz(strtod(s, &s) * cts_cm / 21.));
	if (*s && *++s)
		plotopt(FSYMS, s);
	if (fsize && *s)
		plotopt(mask ? FPLOT : FPLOTN);
	else
		plotopt((mask > 0 && dsize) ? DASH : SOLID);
}
