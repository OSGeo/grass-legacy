#ifndef lint
static char *SCCSID = "@(#)newdir.c	USGS v.1.1";
#endif
/*	convert old machine dependent coast directory
**	into machine independent directory.
*/
# include <stdio.h>
# include "coast.h"
	int
main(argc, argv) char **argv; {
	struct dircty dir;
	int i;

	for (i=0;;++i) {
		if (!fread(&dir, sizeof(struct dircty), 1, stdin))
			break;
		putdir(dir, stdout);
	}
	fprintf(stderr,"%d entries processed\n",i);
}
