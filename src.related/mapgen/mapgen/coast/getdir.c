/* @(#)getdir.c	USGS v.3.1 */
#include <stdio.h>
#include "coast.h"
/* procedure to read transportable coastline
** directory entry */
	int
getdir(w, file) struct dircty *w; FILE *file; {
	short s;
	int i;
	long l, *lp;

	if (feof(file))	/* user trying to recall */
		return(0);
	w->cntrl = getc(file);
	if (feof(file)) /* correct position for eof */
		return(0);
	w->code  = getc(file); /* don't bother with eof 'til end */
	s = getc(file) << 8;
	w->count = s + getc(file);
	lp = &w->location;
	for (i = 0; i < 7; ++i) {
		l = getc(file) << 24;
		l += getc(file) << 16;
		l += getc(file) << 8;
		*lp++ = l + getc(file);
	}
	if (feof(file)) /* bad position for eof--out of sync */
		exit();
	return(1);
}
