
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
 *
 * CCITT Group 4 Facsimile-compatible
 * Compression Scheme Support.
 */
#include "tiffio.h"
#include "tif_fax3.h"

#if USE_PROTOTYPES
static	int Fax4Decode(TIFF*, u_char *, int);
static	int Fax4Encode(TIFF*, u_char *, int);
static	int Fax4Close(TIFF*);
#else
static	int Fax4Decode();
static	int Fax4Encode();
static	int Fax4Close();
#endif

TIFFInitCCITTFax4(tif)
	TIFF *tif;
{
	TIFFInitCCITTFax3(tif);		/* reuse G3 compression */
	tif->tif_decoderow = Fax4Decode;
	tif->tif_encoderow = Fax4Encode;
	tif->tif_close = Fax4Close;
	/*
	 * This magic causes the regular G3 decompression
	 * code to not skip to the EOL mark at the end of
	 * a row (during normal decoding).
	 */
	tif->tif_options |= TIFF_OPT1;
	return (1);
}

/*
 * Decode the requested amount of data.
 */
static int
Fax4Decode(tif, buf, occ)
	TIFF *tif;
	u_char *buf;
	int occ;
{
	int scanline = tif->tif_scanlinesize;
	int imagewidth = tif->tif_dir.td_imagewidth;
	Fax3BaseState *sp = (Fax3BaseState *)tif->tif_data;

	bzero(buf, occ);		/* decoding only sets non-zero bits */
	while (occ > 0) {
		if (!Fax3Decode2DRow(tif, buf, imagewidth))
			return (0);
		bcopy(buf, sp->refline, scanline);
		buf += scanline;
		occ -= scanline;
	}
	return (1);
}

/*
 * Encode the requested amount of data.
 */
static int
Fax4Encode(tif, bp, cc)
	TIFF *tif;
	u_char *bp;
	int cc;
{
	u_char *refline = ((Fax3BaseState *)tif->tif_data)->refline;
	int scanline = tif->tif_scanlinesize;
	int imagewidth = tif->tif_dir.td_imagewidth;

	while (cc > 0) {
		if (!Fax3Encode2DRow(tif, bp, refline, imagewidth))
			return (0);
		bcopy(bp, refline, scanline);
		bp += scanline;
		cc -= scanline;
	}
	return (1);
}

static
Fax4Close(tif)
	TIFF *tif;
{
	if (tif->tif_data) {		/* append EOFB */
		Fax3PutEOL(tif);
		Fax3PutEOL(tif);
		(*tif->tif_encodestrip)(tif);
	}
}
