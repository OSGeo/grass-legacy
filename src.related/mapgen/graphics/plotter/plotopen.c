#ifndef lint
static char *SCCSID = "@(#)plotopen.c	USGS v.4.2";
#endif
/* establish link with 'plotter' */
#include <stdio.h>
#include <varargs.h>
#include "graphics.h"

BASE base;

static FILE *dev_out = NULL;
static FILE *dev_in = NULL;
static FILE *def_out = NULL;
static char envname[] = _GENVB;
static char name[50];

	int /* establish link with 'plotter' */
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
	void
plotend() {
	p2bclos(dev_out, dev_in);
	dev_out = dev_in = NULL;
}
	int /* open defered file */
defopen(va_alist) va_dcl {
	va_list arg;
	char *file, *mode = "w";

	va_start(arg);
	file = va_arg(arg, char *);
	if (*file == '-') {
		mode = file + 1;
		file = va_arg(arg, char *);
	}
	if (dev_out == NULL) base.x = base.y = 0;
	if (def_out != NULL) fclose(def_out);
	va_end(arg);
	return ((def_out = fopen(file,mode)) == NULL ? 1 : 0);
}
	void /* close defered file */
defclose() {
	if (def_out != NULL) fclose(def_out);
	def_out = NULL;
}
	void /* flush all graphics output buffers */
pltflush() {
	if (def_out != NULL) fflush(def_out);
	if (dev_out != NULL) fflush(dev_out);
}
	void /* put byte into graphics output stream */
plotout(c) int c; {
	if (def_out != NULL) putc(c, def_out);
	if (dev_out != NULL) putc(c, dev_out);
}
	int /* get byte from stream */
plotin(c) {
	if (dev_in == NULL) return (P_ACK);
	return (getc(dev_in));
}
