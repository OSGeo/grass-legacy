
/*
 * Copyright (c) 1990 by Sam Leffler.
 * All rights reserved.
 *
 * This file is provided for unrestricted use provided that this
 * legend is included on all tape media and as a part of the
 * software program in whole or part.  Users may copy, modify or
 * distribute this file at will.
 */

/*
 * fixup an 8-bit palette image by scaling the colormap.
 */

#include <stdio.h>
#include "tiffio.h"

#define	CopyField(tag, v) \
    if (TIFFGetField(in, tag, &v)) TIFFSetField(out, tag, v)
#define	CopyField3(tag, v1, v2, v3) \
    if (TIFFGetField(in, tag, &v1, &v2, &v3)) TIFFSetField(out, tag, v1, v2, v3)

main(argc, argv)
	char *argv[];
{
	short bitspersample, samplesperpixel, shortv;
	short imagewidth, imagelength, config = -1;
	u_short compression = -1;
	long rowsperstrip = -1;
	float floatv;
	char *stringv;
	u_long longv;
	u_char *buf;
	int row, s;
	TIFF *in, *out;

	argc--, argv++;
	if (argc < 2) {
		fprintf(stderr, "usage: fixpal input output\n");
		exit(-1);
	}
	in = TIFFOpen(argv[0], "r");
	if (in == NULL)
		exit(-1);
	shortv = -1;
	(void) TIFFGetField(in, TIFFTAG_PHOTOMETRIC, &shortv);
	if (shortv != PHOTOMETRIC_PALETTE) {
		fprintf(stderr, "%s: Expecting a palette image.\n", argv[0]);
		exit(-1);
	}
	out = TIFFOpen(argv[1], "w");
	if (out == NULL)
		exit(-2);
	CopyField(TIFFTAG_SUBFILETYPE, longv);
	CopyField(TIFFTAG_IMAGEWIDTH, imagewidth);
	CopyField(TIFFTAG_IMAGELENGTH, imagelength);
	CopyField(TIFFTAG_BITSPERSAMPLE, bitspersample);
	CopyField(TIFFTAG_COMPRESSION, compression);
	CopyField(TIFFTAG_PHOTOMETRIC, shortv);
	CopyField(TIFFTAG_THRESHHOLDING, shortv);
	CopyField(TIFFTAG_FILLORDER, shortv);
	CopyField(TIFFTAG_ORIENTATION, shortv);
	CopyField(TIFFTAG_SAMPLESPERPIXEL, samplesperpixel);
	CopyField(TIFFTAG_PREDICTOR, shortv);
	CopyField(TIFFTAG_MINSAMPLEVALUE, shortv);
	CopyField(TIFFTAG_MAXSAMPLEVALUE, shortv);
	CopyField(TIFFTAG_XRESOLUTION, floatv);
	CopyField(TIFFTAG_YRESOLUTION, floatv);
	CopyField(TIFFTAG_GROUP3OPTIONS, longv);
	CopyField(TIFFTAG_GROUP4OPTIONS, longv);
	CopyField(TIFFTAG_RESOLUTIONUNIT, shortv);
	CopyField(TIFFTAG_PLANARCONFIG, config);
	if (rowsperstrip <= 0)
		rowsperstrip = (8*1024)/TIFFScanlineSize(out);
	TIFFSetField(out, TIFFTAG_ROWSPERSTRIP,
	    rowsperstrip == 0 ? 1 : rowsperstrip);
	CopyField(TIFFTAG_XPOSITION, floatv);
	CopyField(TIFFTAG_YPOSITION, floatv);
	CopyField(TIFFTAG_GRAYRESPONSEUNIT, shortv);
	{ u_short *graycurve;
	  CopyField(TIFFTAG_GRAYRESPONSECURVE, graycurve);
	}
	CopyField(TIFFTAG_COLORRESPONSEUNIT, shortv);
	{ u_short *red, *green, *blue;
	  CopyField3(TIFFTAG_COLORRESPONSECURVE, red, green, blue);
	}
	CopyField(TIFFTAG_MATTEING, shortv);
	{ u_short *red, *green, *blue;
	  int i;
	  CopyField3(TIFFTAG_COLORMAP, red, green, blue);
#define	SCALE(x)	(((x)*((1L<<16)-1))/255)
	  for (i = (1<<bitspersample)-1; i >= 0; i--) {
		red[i] = SCALE(red[i]);
		green[i] = SCALE(green[i]);
		blue[i] = SCALE(blue[i]);
	  }
	}
	CopyField(TIFFTAG_ARTIST, stringv);
	CopyField(TIFFTAG_IMAGEDESCRIPTION, stringv);
	(void) TIFFGetField(in, TIFFTAG_PLANARCONFIG, &shortv);
	buf = (u_char *)malloc(TIFFScanlineSize(in));
	for (row = 0; row < imagelength; row++) {
		if (TIFFReadScanline(in, buf, row, 0) < 0 ||
		    TIFFWriteScanline(out, buf, row, 0) < 0)
			break;
	}
	(void) TIFFClose(in);
	(void) TIFFClose(out);
}
