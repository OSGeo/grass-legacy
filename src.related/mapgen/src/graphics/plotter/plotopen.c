#ifndef lint
static char *SCCSID = "@(#)plotopen.c	OEMG v.3.1";
#endif
/* establish link with 'plotter' */
#include <stdio.h>
#include "graphics.h"

BASE base;

static FILE *dev_out = NULL;
static FILE *dev_in = NULL;
static FILE *def_out = NULL;
static char envname[] = _GENVB;
static char name[50];

	/* establish link with 'plotter' */
plotopen(argl) char *argl[]; {
	char *a0, *a1, *getenv();

	if (!(a0 = getenv(envname))) {
		fprintf(stderr, "%s environment not avail.\n",envname);
		exit(1);
	}
	for (argl[0] = a1 = name; *a0 && *a0 != ':'; ) *a1++ = *a0++;
	*a1 = '\0';
	argl[1] = "plotter";
	if (def_out == NULL)
		base.x = base.y = 0;
	return(p2bopen(argl, &dev_in, &dev_out));
}
plotend() {
	p2bclos(dev_out, dev_in);
	dev_out = dev_in = NULL;
}
	/* open defered file */
defopen(file) char *file; {
	if (dev_out == NULL) base.x = base.y = 0;
	if (def_out != NULL) fclose(def_out);
	return ((def_out = fopen(file,"w")) == NULL ? 1 : 0);
}
	/* close defered file */
defclose() {
	if (def_out != NULL) fclose(def_out);
	def_out = NULL;
}
	/* flush all graphics output buffers */
pltflush() {
	if (def_out != NULL) fflush(def_out);
	if (dev_out != NULL) fflush(dev_out);
}
	/* put byte into graphics output stream */
plotout(c) int c; {
	if (def_out != NULL) putc(c, def_out);
	if (dev_out != NULL) putc(c, dev_out);
	pltflush();
}
	/* get byte from stream */
plotin(c) {
	if (dev_in == NULL) return (P_ACK);
	return (getc(dev_in));
}
