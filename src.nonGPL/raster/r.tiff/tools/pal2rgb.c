
/*
 * Copyright (c) 1988, 1990 by Sam Leffler.
 * All rights reserved.
 *
 * This file is provided for unrestricted use provided that this
 * legend is included on all tape media and as a part of the
 * software program in whole or part.  Users may copy, modify or
 * distribute this file at will.
 */

#include <stdio.h>
#include "tiffio.h"

#define	howmany(x, y)	(((x)+((y)-1))/(y))
#define	streq(a,b)	(strcmp(a,b) == 0)
#define	CopyField(tag, v) \
    if (TIFFGetField(in, tag, &v)) TIFFSetField(out, tag, v)
#define	CopyField3(tag, v1, v2, v3) \
    if (TIFFGetField(in, tag, &v1, &v2, &v3)) TIFFSetField(out, tag, v1, v2, v3)

static int
checkcmap(n, r, g, b)
	int n;
	u_short *r, *g, *b;
{
	while (n-- > 0)
	    if (*r++ >= 256 || *g++ >= 256 || *b++ >= 256)
		return (16);
	fprintf(stderr, "Warning, assuming 8-bit colormap.\n");
	return (8);
}

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
	u_short *rmap, *gmap, *bmap;
	int row, s;
	int cmap = -1;
	TIFF *in, *out;

	argc--, argv++;
	if (argc < 2)
		usage();
	for (; argc > 2 && argv[0][0] == '-'; argc--, argv++) {
		if (streq(argv[0], "-none")) {
			compression = COMPRESSION_NONE;
			continue;
		}
		if (streq(argv[0], "-packbits")) {
			compression = COMPRESSION_PACKBITS;
			continue;
		}
		if (streq(argv[0], "-lzw")) {
			compression = COMPRESSION_LZW;
			continue;
		}
		if (streq(argv[0], "-contig")) {
			config = PLANARCONFIG_CONTIG;
			continue;
		}
		if (streq(argv[0], "-separate")) {
			config = PLANARCONFIG_SEPARATE;
			continue;
		}
		if (streq(argv[0], "-8bit")) {
			cmap = 8;
			continue;
		}
		if (streq(argv[0], "-16bit")) {
			cmap = 16;
			continue;
		}
		if (streq(argv[0], "-rowsperstrip")) {
			argc--, argv++;
			rowsperstrip = atoi(argv[0]);
			continue;
		}
		usage();
	}
	in = TIFFOpen(argv[0], "r");
	if (in == NULL)
		exit(-1);
	if (!TIFFGetField(in, TIFFTAG_PHOTOMETRIC, &shortv) ||
	    shortv != PHOTOMETRIC_PALETTE) {
		fprintf(stderr, "%s: Expecting a palette image.\n", argv[0]);
		exit(-1);
	}
	if (!TIFFGetField(in, TIFFTAG_COLORMAP, &rmap, &gmap, &bmap)) {
		fprintf(stderr,
		    "%s: No colormap (not a valid palette image).\n",
		    argv[0]);
		exit(-1);
	}
	out = TIFFOpen(argv[1], "w");
	if (out == NULL)
		exit(-2);
	CopyField(TIFFTAG_SUBFILETYPE, longv);
	CopyField(TIFFTAG_IMAGEWIDTH, imagewidth);
	CopyField(TIFFTAG_IMAGELENGTH, imagelength);
	CopyField(TIFFTAG_BITSPERSAMPLE, bitspersample);
	if (bitspersample != 8) {
		fprintf(stderr, "%s: Sorry, can only handle 8-bit images.\n",
		    argv[0]);
		exit(-1);
	}
	if (compression != (u_short)-1)
		TIFFSetField(out, TIFFTAG_COMPRESSION, compression);
	else
		CopyField(TIFFTAG_COMPRESSION, compression);
	TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
	CopyField(TIFFTAG_ORIENTATION, shortv);
	TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, 3);
	CopyField(TIFFTAG_PREDICTOR, shortv);
	CopyField(TIFFTAG_MINSAMPLEVALUE, shortv);
	CopyField(TIFFTAG_MAXSAMPLEVALUE, shortv);
	CopyField(TIFFTAG_XRESOLUTION, floatv);
	CopyField(TIFFTAG_YRESOLUTION, floatv);
	CopyField(TIFFTAG_RESOLUTIONUNIT, shortv);
	if (config != -1)
		TIFFSetField(out, TIFFTAG_PLANARCONFIG, config);
	else
		CopyField(TIFFTAG_PLANARCONFIG, config);
	if (rowsperstrip <= 0)
		rowsperstrip = (8*1024)/TIFFScanlineSize(out);
	TIFFSetField(out, TIFFTAG_ROWSPERSTRIP,
	    rowsperstrip == 0 ? 1 : rowsperstrip);
	CopyField(TIFFTAG_XPOSITION, floatv);
	CopyField(TIFFTAG_YPOSITION, floatv);
	CopyField(TIFFTAG_COLORRESPONSEUNIT, shortv);
	{ u_short *red, *green, *blue;
	  CopyField3(TIFFTAG_COLORRESPONSECURVE, red, green, blue);
	}
	CopyField(TIFFTAG_ARTIST, stringv);
	CopyField(TIFFTAG_IMAGEDESCRIPTION, stringv);
	CopyField(TIFFTAG_MAKE, stringv);
	CopyField(TIFFTAG_MODEL, stringv);
	CopyField(TIFFTAG_SOFTWARE, stringv);
	CopyField(TIFFTAG_DATETIME, stringv);
	CopyField(TIFFTAG_HOSTCOMPUTER, stringv);
	CopyField(TIFFTAG_PAGENAME, stringv);
	CopyField(TIFFTAG_DOCUMENTNAME, stringv);
	(void) TIFFGetField(in, TIFFTAG_PLANARCONFIG, &shortv);
	if (shortv != config && bitspersample != 8 && samplesperpixel > 1) {
		fprintf(stderr,
"Can't handle different planar configuration w/ bits/sample != 8\n");
		exit(-1);
	}
	if (cmap == -1)
		cmap = checkcmap(1<<bitspersample, rmap, gmap, bmap);
	if (cmap == 16) {
		/*
		 * Convert 16-bit colormap to 8-bit.
		 */
		int i;

		for (i = (1<<bitspersample)-1; i > 0; i--) {
#define	CVT(x)		(((x) * 256) / ((1L<<16)-1))
			rmap[i] = CVT(rmap[i]);
			gmap[i] = CVT(gmap[i]);
			bmap[i] = CVT(bmap[i]);
		}
	}
	{ u_char *ibuf, *obuf;
	  register u_char* pp;
	  register int x;
	  ibuf = (u_char*)malloc(TIFFScanlineSize(in));
	  obuf = (u_char*)malloc(TIFFScanlineSize(out));
	  switch (config) {
	  case PLANARCONFIG_CONTIG:
		for (row = 0; row < imagelength; row++) {
			if (!TIFFReadScanline(in, ibuf, row, 0))
				goto done;
			pp = obuf;
			for (x = 0; x < imagewidth; x++) {
				*pp++ = rmap[ibuf[x]];
				*pp++ = gmap[ibuf[x]];
				*pp++ = bmap[ibuf[x]];
			}
			if (!TIFFWriteScanline(out, obuf, row, 0))
				goto done;
		}
		break;
	  case PLANARCONFIG_SEPARATE:
		for (row = 0; row < imagelength; row++) {
			if (!TIFFReadScanline(in, ibuf, row, 0))
				goto done;
			for (pp = obuf, x = 0; x < imagewidth; x++)
				*pp++ = rmap[ibuf[x]];
			if (!TIFFWriteScanline(out, obuf, row, 0))
				goto done;
			for (pp = obuf, x = 0; x < imagewidth; x++)
				*pp++ = gmap[ibuf[x]];
			if (!TIFFWriteScanline(out, obuf, row, 0))
				goto done;
			for (pp = obuf, x = 0; x < imagewidth; x++)
				*pp++ = bmap[ibuf[x]];
			if (!TIFFWriteScanline(out, obuf, row, 0))
				goto done;
		}
		break;
	  }
	  free(ibuf);
	  free(obuf);
	}
done:
	(void) TIFFClose(in);
	(void) TIFFClose(out);
}

usage()
{
	fprintf(stderr, "usage: pal2rgb [options] input output\n");
	fprintf(stderr, "where options are:\n");
	fprintf(stderr,
	    " -contig\tpack samples contiguously (e.g. RGBRGB...)\n");
	fprintf(stderr,
	    " -separate\tstore samples separately (e.g. RRR...GGG...BBB...)\n");
	fprintf(stderr, "\n");
	fprintf(stderr,
	    " -lzw\t\tcompress output with Lempel-Ziv & Welch encoding\n");
	fprintf(stderr,
	    " -packbits\tcompress output with packbits encoding\n");
	fprintf(stderr,
	    " -none\t\tuse no compression algorithm on output\n");
	fprintf(stderr, "\n");
	fprintf(stderr,
	    " -8bit\tassume 8-bit colormap values (instead of 16-bit)\n");
	fprintf(stderr,
	    " -rowsperstrip #\tmake each strip have no more than # rows\n");
	exit(-1);
}
