#ifndef lint
static char *SCCSID = "@(#)TRsetdev.c	AMG v.3.1";
#endif
/* basic control initialization */
# include <stdio.h>
# include <search.h>
# include "TRdev.h"
	extern char
troffnam[];
	void
TRsetdev(s) char *s; { /* load DESC tables */
	int i, n;
	ENTRY item, *hsearch();
	char name[80];
	short *malloc();
	FILE *fid;
	char *ptr;

		/* load device tables */
	strcpy(name, troffnam);
	strcat(name, s);
	strcat(name, "/DESC.out");
	if (!(fid = fopen(name, "r"))) {
		perror(name);
		bomb("failure to open control file");
	}
	else if (!fread(&device, sizeof(struct dev), 1, fid))
		bomb("device length read error");
	else if (!(sizes = malloc(device.filesize)))
		bomb("memory allocation error");
	else if (!fread(sizes, device.filesize, 1, fid))
		bomb("font length read error");
	else if (device.nfonts <= MAX_FONTS) {
		chtab = sizes + device.nsizes + 1;
		chname = (char *)(chtab + device.nchtab);
		ptr = chname + device.lchname;
		for (i = 0; i < device.nfonts; ++i) {
			fonttab[i].font = (struct font *)ptr;
			n = *ptr & 0xff;
			ptr += sizeof(struct font);
			fonttab[i].widths = (BYTE *)ptr; ptr += n;
			fonttab[i].kerning = (BYTE *)ptr; ptr += n;
			fonttab[i].codes = (BYTE *)ptr; ptr += n;
			fonttab[i].index = (BYTE *)ptr;
			ptr += 96 + device.nchtab;
		}
	}
	fclose(fid);

		/* create hash lookup for special characters */
	if (!hcreate(280))
		bomb("Can't allocate hash table");
	for (i = 0; i < device.nchtab; ++i) {
		item.key = chname + chtab[i];
		item.data = (char *)(i + 96);
		if (!hsearch(item, ENTER))
			bomb("Hash table overflow");
	}
}
