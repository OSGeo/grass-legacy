
/*
 * Copyright (c) 1988, 1990 by Sam Leffler.
 * All rights reserved.
 *
 * This file is provided for unrestricted use provided that this
 * legend is included on all tape media and as a part of the
 * software program in whole or part.  Users may copy, modify or
 * distribute this file at will.
 */

/*
 * TIFF Library.
 */
#include <stdio.h>
#include "tiffio.h"

void
#if USE_PROTOTYPES
TIFFWarning(char *module, char *fmt, ...)
#else
/*VARARGS2*/
TIFFWarning(module, fmt, va_alist)
	char *module;
	char *fmt;
	va_dcl
#endif
{
	va_list ap;

	if (module != NULL)
		fprintf(stderr, "%s: ", module);
	fprintf(stderr, "Warning, ");
	VA_START(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, ".\n");
}
