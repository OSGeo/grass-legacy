
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

#define	streq(a,b)	(strcmp((a),(b)) == 0)
#define	CopyField(tag, v) \
	if (TIFFGetField(in, tag, &v)) TIFFSetField(out, tag, v)

/* x% weighting -> fraction of full color */
#define	CVT(x)	(((x)*255)/100)
int	RED = CVT(28);		/* 28% */
int	GREEN = CVT(59);	/* 59% */
int	BLUE = CVT(11);		/* 11% */

usage()
{
	fprintf(stderr,
    "usage: tiff2bw [-r red%%] [-g green%%] [-b blue%%] input output\n");
	exit(1);
}

main(argc, argv)
	int argc;
	char **argv;
{
	TIFF *in, *out;
	unsigned short w, h, samplesperpixel, bitspersample, shortv, config;
	register int row, s;
	unsigned char *inbuf, *outbuf;
	char thing[1024];
	int rowsperstrip, c;
	extern int optind;
	extern char *optarg;

	while ((c = getopt(argc, argv, "r:g:b:")) != -1)
		switch (c) {
		case 'r':
			RED = CVT(atoi(optarg));
			break;
		case 'g':
			GREEN = CVT(atoi(optarg));
			break;
		case 'b':
			BLUE = CVT(atoi(optarg));
			break;
		case '?':
			usage();
			/*NOTREACHED*/
		}
	if (argc - optind < 2)
		usage();
	in = TIFFOpen(argv[optind], "r");
	if (in == NULL)
		exit(-1);
	TIFFGetField(in, TIFFTAG_SAMPLESPERPIXEL, &samplesperpixel);
	if (samplesperpixel != 3) {
		fprintf(stderr, "%s: Not a color image.\n", argv[0]);
		exit(-1);
	}
	TIFFGetField(in, TIFFTAG_BITSPERSAMPLE, &bitspersample);
	if (bitspersample != 8) {
		fprintf(stderr,
		    " %s: Sorry, only handle 8-bit samples.\n", argv[0]);
		exit(-1);
	}
	out = TIFFOpen(argv[optind+1], "w");
	if (out == NULL)
		exit(-1);
	CopyField(TIFFTAG_IMAGEWIDTH, w);
	CopyField(TIFFTAG_IMAGELENGTH, h);
	TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, 8);
	TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, 1);
	TIFFSetField(out, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
	TIFFSetField(out, TIFFTAG_COMPRESSION, COMPRESSION_LZW);
	TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
	sprintf(thing, "B&W version of %s", argv[optind]);
	TIFFSetField(out, TIFFTAG_IMAGEDESCRIPTION, thing);
	TIFFSetField(out, TIFFTAG_SOFTWARE, "tiff2bw");
	CopyField(TIFFTAG_ORIENTATION, shortv);
	outbuf = (unsigned char *)malloc(TIFFScanlineSize(out));
	rowsperstrip = (8*1024)/TIFFScanlineSize(out);
	TIFFSetField(out, TIFFTAG_ROWSPERSTRIP,
	    rowsperstrip == 0 ? 1 : rowsperstrip);
	TIFFGetField(in, TIFFTAG_PLANARCONFIG, &config);
	switch (config) {
	case PLANARCONFIG_CONTIG:
		inbuf = (unsigned char *)malloc(TIFFScanlineSize(in));
		for (row = 0; row < h; row++) {
			if (TIFFReadScanline(in, inbuf, row, 0) < 0)
				break;
			compresscontig(outbuf, inbuf, w);
			if (TIFFWriteScanline(out, outbuf, row, 0) < 0)
				break;
		}
		break;
	case PLANARCONFIG_SEPARATE: {
		int rowbytes = TIFFScanlineSize(in);
		inbuf = (unsigned char *)malloc(3*rowbytes);
		for (row = 0; row < h; row++) {
			for (s = 0; s < 3; s++)
				if (TIFFReadScanline(in,
				    inbuf+s*rowbytes, row, s) < 0)
					 exit(-1);
			compresssep(outbuf,
			    inbuf, inbuf+rowbytes, inbuf+2*rowbytes, w);
			if (TIFFWriteScanline(out, outbuf, row, 0) < 0)
				break;
		}
	}
	}
	TIFFClose(out);
}

compresscontig(out, rgb, n)
	register unsigned char *out, *rgb;
	register int n;
{
	register int v, red = RED, green = GREEN, blue = BLUE;

	while (n-- > 0) {
		v = red*(*rgb++);
		v += green*(*rgb++);
		v += blue*(*rgb++);
		*out++ = v>>8;
	}
}

compresssep(out, r, g, b, n)
	register unsigned char *out, *r, *g, *b;
	register int n;
{
	register int red = RED, green = GREEN, blue = BLUE;

	while (n-- > 0)
		*out++ = (red*(*r++) + green*(*g++) + blue*(*b++)) >> 8;
}
