#ifndef lint
static char *SCCSID = "@(#)TRloadfont.c	AMG v.3.1";
#endif
/* set mapping for troff font number to internal location */
# include "TRdev.h"
	void
TRloadfont(n, name) char *name; {
	int i;

	for (i = 0; i < device.nfonts; ++i)
		if (!strcmp(name, fonttab[i].font->namefont)) {
			map[n] = i;
			/* tell user program about it */
			loadfont(n, fonttab[i].font->intname);
			break;
		}
	if (i >= device.nfonts)
		bomb("requested font not in table");
}
