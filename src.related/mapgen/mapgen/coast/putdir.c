/* @(#)putdir.c	USGS v.3.1 */
#include <stdio.h>
#include "coast.h"
/* procedure to write transportable coastline
** directory entry */
	void
putdir(w, file) struct dircty *w; FILE *file; {
	int i;
	long l, *lp;

	putc(w->cntrl, file);
	putc(w->code, file);
	putc(w->count >> 8, file);
	putc(w->count, file);
	lp = &w->location;
	for (i = 0; i < 7; ++i) {
		l = *lp++;
		putc(l >> 24, file);
		putc(l >> 16, file);
		putc(l >>  8, file);
		putc(l, file);
	}
}
