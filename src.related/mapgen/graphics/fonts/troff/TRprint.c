#ifndef lint
static char *SCCSID = "@(#)TRprint.c	AMG v.3.1";
#endif
# include "TRdev.h"
	static int
Nlast = -1;
	void /* map incoming troff character to user character */
TRprint(n) {
	static c, i, p, N;

	if (c = font.index[n]) {
		if (Nlast >= 0) {
			setfont(nfont);
			Nlast = -1;
		}
		print(H, V, font.codes[c]);
	} else for (i = 1; i <= device.nfonts; ++i) {
		N = map[i];
		if (fonttab[N].font->specfont && (c = fonttab[N].index[n])) {
			if (!strcmp(fonttab[N].font->intname,"SY")) {
				p = fonttab[N].codes[c];
				sprint(H, V, p);
			} else if (p = fonttab[N].codes[c]) {
				if (N != Nlast) {
					setfont(i);
					Nlast = N;
				}
				print(H, V, p);
			}
			break;
		}
	}
}
