#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_flush.c,v 1.7 90/10/08 17:17:46 sam Exp $";
#endif

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
#include "tiffio.h"

TIFFFlush(tif)
	TIFF *tif;
{

	if (tif->tif_mode != O_RDONLY) {
		if (tif->tif_rawcc > 0 && !TIFFFlushData(tif))
			return (0);
		if ((tif->tif_flags & TIFF_DIRTYDIRECT) &&
		    !TIFFWriteDirectory(tif))
			return (0);
	}
	return (1);
}
